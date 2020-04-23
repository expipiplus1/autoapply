module AutoApply
  ( autoapply
  , autoapplyDecs
  ) where

import           Control.Applicative
import           Control.Arrow                  ( (>>>) )
import           Control.Monad
import           Control.Monad.Logic            ( LogicT
                                                , observeManyT
                                                )
import           Control.Monad.Logic.Class      ( ifte )
import           Control.Monad.Trans           as T
import           Control.Monad.Trans.Except
import           Control.Unification
import           Control.Unification.IntVar
import           Control.Unification.Types
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Fixedpoint
import           Data.Maybe
import           Data.Traversable
import           Language.Haskell.TH
import           Language.Haskell.TH.Desugar

data Command = Command
  { cName :: Name
  , cType :: DType
  }
  deriving (Show)

data Given = Given
  { gName :: Name
  , gType :: DType
  }
  deriving (Show)

autoapply :: [Name] -> Name -> Q Exp
autoapply givens fun = do
  getterInfos <- for givens $ \n -> dsReify n >>= \case
    Just (DVarI name ty _) -> pure $ Given name ty
    _                      -> fail $ "Getter isn't a value " <> show n
  funInfo <- dsReify fun >>= \case
    Just (DVarI name ty _) -> pure (Command name ty)
    _                      -> fail $ "Function isn't a value " <> show fun
  autoapply1 getterInfos funInfo

autoapplyDecs :: (String -> String) -> [Name] -> [Name] -> Q [Dec]
autoapplyDecs getNewName givens funs = do
  getterInfos <- for givens $ \n -> dsReify n >>= \case
    Just (DVarI name ty _) -> pure $ Given name ty
    _                      -> fail $ "Getter isn't a value " <> show n
  funInfos <- for funs $ \n -> dsReify n >>= \case
    Just (DVarI name ty _) -> pure (Command name ty)
    _                      -> fail $ "Function isn't a value " <> show n
  let mkFun fun = do
        exp' <- autoapply1 getterInfos fun
        pure $ FunD (mkName . getNewName . nameBase . cName $ fun)
                    [Clause [] (NormalB exp') []]
  traverse mkFun funInfos

autoapply1 :: [Given] -> Command -> Q Exp
autoapply1 getters fun = do
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
                        _ <- unify m' cmdM
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
        for (zip args as) $ \case
          (_, Just p ) -> pure p
          (t, Nothing) -> (`Argument` t) <$> liftQ (newName "a")

  argProvenances <-
    note "\"Impossible\" Finding argument provenances failed"
    .   listToMaybe
    =<< observeManyT 1 genProvs
  unless (length argProvenances == length args) $ fail
    "\"Impossible\", incorrect number of argument provenances were found"

  let assignGetter = \case
        BoundPure _ _ -> Nothing
        Bound     n g -> Just $ BindS (VarP n) (VarE (gName g))
        Argument  _ _ -> Nothing
      bs   = catMaybes (assignGetter <$> argProvenances)
      ret' = applyDExp
        (DVarE (cName fun))
        (argProvenances <&> \case
          Bound     n _           -> DVarE n
          BoundPure _ (Given n _) -> DVarE n
          Argument  n _           -> DVarE n
        )
  exp' <- dsDoStmts (bs <> [NoBindS (sweeten ret')])
  -- Typing the arguments here is important
  pure $ LamE [ SigP (VarP n) (sweeten t) | Argument n t <- argProvenances ]
              (sweeten exp')

data ArgProvenance
  = Bound Name Given
  | BoundPure Name Given
    | Argument Name DType
  deriving (Show)

----------------------------------------------------------------
-- Haskell types as a fixed point of TypeF
----------------------------------------------------------------

data TypeF a
  = AppF a a
  | VarF Name
  | ConF Name
  | ArrowF
  | LitF TyLit
  deriving (Show, Functor, Foldable, Traversable)

-- | TODO: Derive this with generics
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
typeDtoF = traverse go . stripForall
 where
  go = \case
    DForallT{} -> fail "TODO: Higher ranked types"
    DAppT l r  -> do
      l' <- go l
      r' <- go r
      pure $ Fix (AppF l' r')
    DAppKindT t _ -> go t
    DSigT     t _ -> go t
    DVarT n       -> pure . Fix $ VarF n
    DConT n       -> pure . Fix $ ConF n
    DArrowT       -> pure . Fix $ ArrowF
    DLitT l       -> pure . Fix $ LitF l
    DWildCardT    -> fail "TODO: Wildcards"

varBndrName :: DTyVarBndr -> Name
varBndrName = \case
  DPlainTV n    -> n
  DKindedTV n _ -> n

-- | Raise foralls on the spine of the function type to the top
--
-- For example @forall a. a -> forall b. b@ becomes @forall a b. a -> b@
raiseForalls :: DType -> DType
raiseForalls = uncurry3 DForallT . go
 where
  go = \case
    DForallT vs ctx t ->
      let (vs', ctx', t') = go t in (vs <> vs', ctx <> ctx', t')
    l :~> r -> let (vs, ctx, r') = go r in (vs, ctx, l :~> r')
    t       -> ([], [], t)

pattern (:~>) :: DType -> DType -> DType
pattern l :~> r = DArrowT `DAppT` l `DAppT` r

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

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

stripForall :: DType -> ([Name], DType)
stripForall = raiseForalls >>> \case
  DForallT vs _ ty -> (varBndrName <$> vs, ty)
  ty               -> ([], ty)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

note :: MonadFail m => String -> Maybe a -> m a
note s = maybe (fail s) pure
