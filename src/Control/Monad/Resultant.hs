{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}

module Control.Monad.Resultant
  ( Resultant
  , ResultantT
  , runResultantT
  , ResultantMonad
    (point, fail, fromEither, fromMaybe, mapEither, updateError
    , recoverWith )
  , lift
  , runResultant
  , getState
  , setState
  , withState
  , updateState
  , Rise(..)
  , safeIO
  ) where

import Prelude hiding (fail)
import Data.Functor.Identity (Identity(runIdentity))
import Control.Exception (Exception, SomeException, try)

import Control.Monad.Result (Result(Ok, Err), mapError, swapResult, toResult)


-- Resultant
-- ---------------------------------------------------------------------

type Resultant st e = ResultantT Identity st e 

runResultant :: Resultant st e a -> st -> (st, Result e a)
runResultant m st = runIdentity $ runResultantT m st


-- ResultantMonad
-- ---------------------------------------------------------------------

class Monad m => ResultantMonad m e where
  point   :: Result e a -> m a
  reflect :: m a -> m (Result e a)

  fail :: e -> m a
  fail = point . Err

  fromEither :: Either e a -> m a
  fromEither (Right x)  = return x
  fromEither (Left err) = fail err

  fromMaybe :: e -> Maybe a -> m a
  fromMaybe _ (Just x) = return x
  fromMaybe err _      = fail err

  mapEither :: (e' -> e) -> Either e' a -> m a
  mapEither _ (Right x)  = return x
  mapEither f (Left err) = fail $ f err

  updateError :: (e -> e) -> m a -> m a
  updateError f m = reflect m >>= point . mapError f 

  recoverWith :: (e -> m a) -> m a -> m a
  recoverWith f m = reflect m >>= \r -> case r of
    Ok x  -> return x
    Err e -> f e


instance ResultantMonad (Result e) e where
  point   = id 
  reflect = return
  fail s  = Err s

-- ResultantT
-- ---------------------------------------------------------------------

newtype ResultantT m st e a = ResultantT { runResultantT :: st -> m (st, Result e a) }

instance Monad m => Functor (ResultantT m st e) where
  fmap f x = ResultantT $ \st -> do (st', r) <- runResultantT x st
                                    return (st', fmap f r)

instance Monad m => Applicative (ResultantT m st e) where
  pure x    = ResultantT $ \st -> return (st, pure x)
  ff <*> fx = ResultantT $ \st -> do (st' , rf) <- runResultantT ff st
                                     (st'', rx) <- runResultantT fx st'
                                     return (st'', rf <*> rx)

instance Monad m => Monad (ResultantT m st e) where
  return   = pure
  ma >>= f = ResultantT $ \st -> do (st', ra) <- runResultantT ma st
                                    rb        <- return (f <$> ra)
                                    case rb of Ok mb -> runResultantT mb st'
                                               Err e -> return (st', Err e)

instance Monad m => ResultantMonad (ResultantT m st e) e where
  point x = ResultantT $ \st -> return (st, x)
  reflect m = ResultantT $ \st -> do
    (st', r) <- runResultantT m st
    return (st', return r)

class Exceptional e where
  convertException :: SomeException -> e

instance Exceptional [Char] where
  convertException = show

--class Risen m where
--  rise :: m a -> 

class (Monad (r i s e), Monad i, Exceptional e) => Rise (r :: (* -> *) -> * -> * -> * -> *) i s e where
  rRun :: r i s e a -> s -> i (s, Result e a)
  rCon :: (s -> i (s, Result e a)) -> r i s e a

  rPoint :: Result e a -> r i s e a
  rPoint x = rCon $ \s -> return (s, x)

  rLift :: i a -> r i s e a
  rLift m = rCon $ \s -> fmap ((,) s . return) m

  rJoin :: i (r i s e a) -> r i s e a
  rJoin m = rCon $ \s -> (m >>= flip rRun s)

  rResult :: r i s e a -> r i s e (Result e a)
  rResult m = rCon $ \s -> 
    do (s', r) <- rRun m s
       return (s', return r)

  rMap :: (a -> b) -> r i s e a -> r i s e b
  rMap f x = fmap f x

  rMapE :: Rise r i s e' => (e -> e') -> r i s e a -> r i s e' a
  rMapE f m = rCon $ \s -> 
    do (s', r) <- rRun m s
       return (s', mapError f r)

  rFail :: e -> r i s e a
  rFail = rPoint . Err

  rEither :: Either e a -> r i s e a
  rEither lr = case lr of
    Left l  -> rFail l
    Right r -> return r


safeIO :: Rise r IO s e => IO a -> r IO s e a
safeIO m = 
  let r = (toResult <$> try m) :: IO (Result SomeException _)
  in rJoin $ fmap (rPoint . mapError convertException) r

instance (Monad i, Exceptional e) => Rise ResultantT i s e where
  rRun = runResultantT
  rCon = ResultantT


-- Utility functions
-- ---------------------------------------------------------------------

-- | Return the current state
getState :: Monad m => ResultantT m st e st
getState = ResultantT $ \st -> return (st, return st)

-- | Replace the current state with the given value
setState :: Monad m => st -> ResultantT m st e ()
setState x = ResultantT $ \_ -> return (x, return ())

-- | Run a computation with the given state, discarding the resulting state.
withState :: Monad m => st -> ResultantT m st e a -> ResultantT m st e a
withState localState x = 
  ResultantT $ \st -> do (_, r) <- runResultantT x localState
                         return (st, r)

-- | Transform the current state with the provided function
updateState :: Monad m => (st -> st) -> ResultantT m st e ()
updateState f = getState >>= setState . f

-- | Lift a value from the underlying monad into the Resultant
lift :: Monad m => m a -> ResultantT m st e a
lift m =
  let toResult st x = (st, return x)
  in ResultantT $ \st -> toResult st <$> m

