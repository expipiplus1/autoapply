{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Types where

import           AutoApply
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Kind

----------------------------------------------------------------
-- Values to test with
----------------------------------------------------------------

newtype Resource a = Resource a

myBracket
  :: MyConstraint m
  => MyMonadT m a
  -> (a -> MyMonadT m c)
  -> (a -> MyMonadT m b)
  -> MyMonadT m b
myBracket = undefined

class MonadResource (m :: Type -> Type) where
data ReleaseKey
myAllocate :: MonadResource m => IO a -> (a -> IO ()) -> m (ReleaseKey, a)
myAllocate = undefined

data Foo
data Bar

class Monad m => MyConstraint (m :: Type -> Type) where

newtype MyMonadT a b = MyMonadT (a b)
  deriving newtype (Functor, Applicative, Monad)

getFooSem :: MyConstraint m => MyMonadT m Foo
getFooSem = undefined

getBar :: m Bar
getBar = undefined

getBarIO :: IO Bar
getBarIO = undefined

aBar :: Bar
aBar = undefined

-- $
-- >>> :set -XTemplateHaskell

----------------------------------------------------------------
-- Tests
----------------------------------------------------------------

-- | Two monadic bindings
--
-- >>> x = $(autoapply ['getFooSem, 'getBar] 'test1)
-- >>> :t x
-- x :: MyConstraint m => MyMonadT m ()
test1 :: Foo -> Bar -> m ()
test1 = undefined

-- | 'aBar' is substituted and 'Foo' is left as an argument
--
-- >>> x = $(autoapply ['aBar] 'test2)
-- >>> :t x
-- x :: Foo -> m ()
test2 :: Foo -> Bar -> m ()
test2 = undefined

-- | 'id' gets substituted at @(a -> a)@
--
-- >>> $(autoapply ['id] 'test3)
-- ()
test3 :: (a -> a) -> ()
test3 = const ()

-- | 'id' gets substituted at @(a -> b)@
--
-- >>> $(autoapply ['id] 'test4)
-- ()
test4 :: (a -> b) -> ()
test4 = const ()

-- | @aBar :: Bar@ gets substituted at @a@

-- >>> $(autoapply ['aBar] 'test5)
-- ()
test5 :: a -> ()
test5 = const ()

-- | 'id' does not get substituted at @(forall a b. a -> b)@
--
-- >>> x = $(autoapply ['id] 'test6)
-- >>> :t x
-- x :: (forall a b. a -> b) -> ()
--
-- 'undefined' does
-- >>> x = $(autoapply ['undefined] 'test6)
-- >>> :t x
-- x :: ()
test6 :: (forall a b. a -> b) -> ()
test6 = const ()
autoapplyDecs (<> "'") ['id] ['test6]
test6' :: (forall a b. a -> b) -> ()

-- | @aBar :: Bar@ does not get substituted at @forall a. a@
--
-- >>> x = $(autoapply ['aBar] 'test7)
-- >>> :t x
-- x :: (forall a. a) -> ()
test7 :: (forall a. a) -> ()
test7 = const ()

-- | 'id' is instantiated twice at different types
-- >>> $(autoapply ['id] 'test8)
-- ()
test8 :: (Foo -> Foo) -> (Bar -> Bar) -> ()
test8 = const (const ())

-- | The return type changes
--
-- >>> x = $(autoapply ['reverse] 'test9)
-- >>> :t x
-- x :: [a] -> [a]
test9 :: ([a] -> b) -> [a] -> b
test9 = test9

-- | Two monadic bindings with types incompatible with one another
--
-- >>> x = $(autoapply ['getFooSem, 'getBarIO] 'test10)
-- >>> :t x
-- x :: MyConstraint m => Bar -> MyMonadT m ()
--
-- Responds to the order of types in the applying function, not the order types
-- of values to be passed.
-- >>> x = $(autoapply ['getBarIO, 'getFooSem] 'test10)
-- >>> :t x
-- x :: MyConstraint m => Bar -> MyMonadT m ()
test10 :: Foo -> Bar -> m ()
test10 = undefined

-- | Monadic binding with incompatible type
--
-- >>> x = $(autoapply ['getFooSem, 'getBarIO] 'test11)
-- >>> :t x
-- x :: Foo -> IO ()
test11 :: Foo -> Bar -> IO ()
test11 = undefined

-- | Several instantiations of the same function
--
-- >>> x = $(autoapply ['bracket, 'getFooSem, 'aBar] 'test12)
-- >>> :t x
-- x :: Foo -> (a -> IO c) -> IO c
--
-- >>> x = $(autoapply ['myBracket, 'aBar] 'test12)
-- >>> :t x
-- x :: MyConstraint m => Foo -> (a -> MyMonadT m b) -> MyMonadT m b
--
-- >>> x = $(autoapply ['myAllocate, 'aBar] 'test12)
-- >>> :t x
-- x :: MonadResource m => Foo -> m (ReleaseKey, a)
test12 :: (m a -> (a -> m b) -> c) -> Foo -> Bar -> c
test12 = undefined

-- |
--
-- >>> x = $(autoapply ['exitFailure] 'liftIO)
-- >>> :t x
-- x :: MonadIO m => m a
exitFailure :: IO a
exitFailure = undefined

