{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
  ) where

import Prelude hiding (fail)
import Data.Functor.Identity (Identity(runIdentity))

import Control.Monad.Result (Result(Ok, Err), mapError)


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
--  fail s = Err s


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

