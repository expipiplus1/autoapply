{-# language TupleSections #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TemplateHaskellQuotes #-}

module AutoApply
  where

import           Control.Applicative
import           Control.Arrow                  ( (>>>) )
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.Logic            ( LogicT
                                                , observeManyT
                                                , observeT
                                                )
import           Control.Monad.Logic.Class      ( ifte )
import           Control.Monad.Trans           as T
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Unification
import           Control.Unification.IntVar
import           Control.Unification.Types
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Fixedpoint
import           Data.Generics.Uniplate.Data
import           Data.List                      ( find )
import           Data.Maybe
import           Data.Traversable
import           Data.Tuple.Extra               ( uncurry3 )
import           Language.Haskell.TH
import           Language.Haskell.TH.Desugar
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

data Command = Command
  { cName :: Name
  , cType :: DType
  }
  deriving Show

data Given = Given
  { gName :: Name
  , gType :: DType
  }
  deriving (Show)

stripForall = \case
  DForallT vs _ ty -> (fmap varBndrName vs, ty)
  ty               -> ([], ty)

pattern (:~>) :: DType -> DType -> DType
pattern l :~> r = DArrowT `DAppT` l `DAppT` r

remix :: [Name] -> [Name] -> Q [Dec]
remix givens funs = do
  getterInfos <- for givens $ \n -> dsReify n >>= \case
    Just (DVarI name ty _) -> pure $ Given name ty
    _                      -> fail $ "Getter isn't a value " <> show n
  funInfos <- for funs $ \n -> dsReify n >>= \case
    Just (DVarI name ty _) -> pure (Command name ty)
    _                      -> fail $ "Function isn't a value " <> show n
  -- runIO $
  --   writeFile "/home/j/projects/vulkan/a" (show (getterInfos,funInfos))
  sweeten <$> traverse (remix1 getterInfos) funInfos

remix1 :: [Given] -> Command -> Q DDec
remix1 getters fun = do
  -- In this function we:
  --
  -- - Instantiate the command type with new unification variables
  -- - Split it into arguments and return type
  -- - Try to unify it with every 'Given' at every argument
  --   - If we can unify the monad of the 'Given' with that of the functions and
  --     unify the argument type, use that.
  --   - If nothing matches we just use an 'Argument'
  -- - Take the first result of all these tries

  let (fmap varBndrName -> cmdVars, _preds, args, ret) = unravel (cType fun)
      defaultMaybe m = ifte m (pure . Just) (pure Nothing)
  let liftQ :: Q a -> IntBindingT TypeF (LogicT Q) a
      liftQ = T.lift . T.lift

      genProvs :: LogicT Q [ArgProvenance]
      genProvs = evalIntBindingT $ do
        instArgs <- traverse (inst cmdVars . snd <=< liftQ . typeDtoF) args
        instRet  <- inst cmdVars . snd <=< liftQ . typeDtoF $ ret

        -- This is @Just (m, a)@ when m is Applicative
        retMonad <- case ret of
          DAppT m a -> liftQ (isInstance ''Applicative [sweeten m]) >>= \case
            False -> pure Nothing
            True  -> do
              m' <- inst cmdVars . snd <=< liftQ . typeDtoF $ m
              a' <- inst cmdVars . snd <=< liftQ . typeDtoF $ a
              pure $ Just (m', a')
          _ -> pure Nothing

        -- A list of (type to unify, predicate to use this match, the given
        -- providing the value).
        --
        -- The predicate is there to make sure we only match unifiable monads
        instGivens <- fmap concat . for getters $ \g@Given {..} -> do
          -- The Given applied as is
          nonApp <- do
            instTy <- uncurry inst <=< liftQ . typeDtoF $ gType
            v      <- liftQ $ newName "g"
            pure (instTy, pure (), BoundPure v g)
          -- The given, but in an applicative context, only possible if we can
          -- unify the monad and there is a Monad instance
          app <- case stripForall gType of
            (vars, DAppT m a) | Just (cmdM, _) <- retMonad ->
              liftQ (isInstance ''Applicative [sweeten m]) >>= \case
                False -> pure Nothing
                True  -> do
                  m' <- inst vars . snd <=< liftQ . typeDtoF $ m
                  a' <- inst vars . snd <=< liftQ . typeDtoF $ a
                  v  <- liftQ $ newName "g"
                  let predicate = do
                        unify m' cmdM
                        pure ()
                  pure $ Just (a', predicate, Bound v g)
            _ -> pure Nothing
          pure ([nonApp] <> toList app)

        as <- for instArgs $ \argTy ->
          defaultMaybe . asum $ instGivens <&> \(givenTy, predicate, g) ->
            runExceptT
                (do
                  predicate
                  freshGivenTy <- freshen givenTy
                  unify freshGivenTy argTy
                )
              >>= \case
                    Left  (_ :: UFailure TypeF IntVar) -> empty
                    Right _                            -> pure g
        for as $ \case
          Just p  -> pure p
          Nothing -> Argument <$> liftQ (newName "a")

  argProvenances <-
    note "\"Impossible\" Finding argument provenances failed"
    .   listToMaybe
    =<< observeManyT 1 genProvs
  unless (length argProvenances == length args) $ fail
    "\"Impossible\", incorrect number of argument provenances were found"

  let assignGetter = \case
        BoundPure n g -> Nothing
        Bound     n g -> Just $ BindS (VarP n) (VarE (gName g))
        Argument _    -> Nothing
      bs  = catMaybes (assignGetter <$> argProvenances)
      ret = foldl
        AppE
        (VarE (cName fun))
        (argProvenances <&> \case
          Bound     n _           -> VarE n
          BoundPure _ (Given n _) -> VarE n
          Argument n              -> VarE n
        )
  exp <- dsDoStmts (bs <> [NoBindS ret])
  pure . DLetDec $ DFunD
    (mkName (nameBase (cName fun) <> "'"))
    [DClause [ DVarP n | Argument n <- argProvenances ] exp]

data ArgProvenance
  = Bound Name Given
  | BoundPure Name Given
  | Argument Name
  deriving (Show)

note :: String -> Maybe a -> Q a
note s = maybe (fail s) pure

----------------------------------------------------------------
-- Subsumes
----------------------------------------------------------------

-- | Does one type subsume another. The subsuming type must be quantified.
--
-- Doesn't handle higher rank types.
subsumes' :: DType -> DType -> Q Bool
subsumes' t1 t2 = do
  r <- evalIntBindingT $ do
    (vs1, t1f) <- T.lift $ typeDtoF =<< expandType t1
    (_  , t2f) <- T.lift $ typeDtoF =<< expandType t2
    t1i        <- inst vs1 t1f
    t2i        <- inst [] t2f
    runExceptT $ subsumes t1i t2i
  case r of
    Left  (f :: UFailure TypeF IntVar) -> fail (show f)
    Right s                            -> pure s

-- | Print if n1 subsumes n2 along with their types.
subsumesDebug :: Name -> Name -> Q ()
subsumesDebug n1 n2 = do
  Just (DVarI _ t1 _) <- dsReify n1
  Just (DVarI _ t2 _) <- dsReify n2
  -- runIO . print . unravel $ t1
  -- runIO . print . unravel $ t2
  j                   <- subsumes' t1 t2 >>= \case
    False -> pure " doesn't subsume "
    True  -> pure " subsumes "
  let s = show . ppr . sweeten
  runIO . putStrLn $ s t1 <> "\n  " <> j <> "\n  " <> s t2

----------------------------------------------------------------
-- Haskell types as a fixed point over TypeF
----------------------------------------------------------------

data TypeF a
  = AppF a a
  | VarF Name
  | ConF Name
  | ArrowF
  | LitF TyLit
  deriving (Show, Functor, Foldable, Traversable)

instance Unifiable TypeF where
  zipMatch (AppF l1 r1) (AppF l2 r2) =
    Just (AppF (Right (l1, l2)) (Right (r1, r2)))
  zipMatch (VarF n1) (VarF n2) | n1 == n2 = Just (VarF n1)
  zipMatch (ConF n1) (ConF n2) | n1 == n2 = Just (ConF n1)
  zipMatch ArrowF ArrowF                  = Just ArrowF
  zipMatch (LitF l1) (LitF l2) | l1 == l2 = Just (LitF l1)
  zipMatch _ _                            = Nothing

-- | Returns the type as a @Fix TypeF@ along with any quantified names. Drops
-- any context.
typeDtoF :: MonadFail m => DType -> m ([Name], Fix TypeF)
typeDtoF = raiseForalls >>> \case
  DForallT vs _ ty -> (varBndrName <$> vs, ) <$> go ty
  ty               -> ([], ) <$> go ty
 where
  go = \case
    DForallT{} -> fail "TODO: Higher ranked types"
    DAppT l r  -> do
      l <- go l
      r <- go r
      pure $ Fix (AppF l r)
    DAppKindT t _ -> go t
    DSigT     t _ -> go t
    DVarT n       -> pure . Fix $ VarF n
    DConT n       -> pure . Fix $ ConF n
    DArrowT       -> pure . Fix $ ArrowF
    DLitT l       -> pure . Fix $ LitF l
    DWildCardT    -> fail "TODO: Wildcards"

varBndrName = \case
  DPlainTV n    -> n
  DKindedTV n _ -> n

-- | Raise foralls on the spine of the function type to the top
--
-- For examples @forall a. a -> forall b. b@ becomes @forall a b. a -> b@
raiseForalls :: DType -> DType
raiseForalls = uncurry3 DForallT . go
 where
  go = \case
    DForallT vs ctx t ->
      let (vs', ctx', t') = go t in (vs <> vs', ctx <> ctx', t')
    l :~> r -> let (vs, ctx, r') = go r in (vs, ctx, l :~> r')
    t       -> ([], [], t)

-- | Instantiate a type with unification variables
inst
  :: BindingMonad TypeF IntVar m
  => [Name]
  -> Fix TypeF
  -> m (UTerm TypeF IntVar)
inst ns t = do
  vs <- sequence [ (n, ) <$> freeVar | n <- ns ]
  let go (Fix f) = case f of
        AppF l r                       -> UTerm (AppF (go l) (go r))
        VarF n | Just v <- lookup n vs -> UVar v
        VarF n                         -> UTerm (VarF n)
        ConF n                         -> UTerm (ConF n)
        ArrowF                         -> UTerm ArrowF
        LitF l                         -> UTerm (LitF l)
  pure $ go t
