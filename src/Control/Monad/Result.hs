module Control.Monad.Result
  ( Result(Ok, Err)
  , mapError
  , get
  ) where


-- Result type
-- ---------------------------------------------------------------------

data Result e a
  = Ok a
  | Err e
  deriving (Show)

instance Functor (Result e) where
  fmap f (Ok x)  = Ok (f x)
  fmap _ (Err e) = Err e

instance Applicative (Result e) where
  pure x        = Ok x
  (Err e) <*> _ = Err e
  (Ok f)  <*> x = fmap f x

instance Monad (Result e) where
  return        = pure
  (Err e) >>= _ = Err e
  (Ok x)  >>= f = f x


-- Utility functions
-- ---------------------------------------------------------------------

-- | Transform the error value of a Result
mapError :: (e1 -> e2) -> Result e1 a -> Result e2 a
mapError _ (Ok x)  = Ok x
mapError f (Err e) = Err $ f e

-- | Unsafely attempt to retrieve the value from a successful Result
get :: Show e => Result e a -> a
get (Ok x)  = x
get (Err e) = error (show e)



