module Util.ReSpec where

{- ReSpec: Resultant-based test specifications -}

import Prelude hiding (fail)
import Control.Monad (when)
import System.Exit (exitFailure)

import Control.Monad.Result
import Control.Monad.Resultant


type Spec = ResultantT IO State String

data State = State
  { stFailed :: Int
  , stTotal  :: Int
  }

initState :: State
initState = State 0 0

testMain :: Spec () -> IO ()
testMain m = do
  (_, r) <- runResultantT m initState
  case r of
    Ok _ -> return ()
    Err e -> putStrLn e >> exitFailure

spec :: String -> Spec () -> Spec ()
spec desc action = 
  let
    onFail :: String -> Spec ()
    onFail err = do
      safeIO $ putStrLn "FAIL"
      safeIO . putStrLn $ "Test failed with message: " ++ err
      updateFailed (+1)
  in do
    n <- show <$> nextTest
    safeIO . putStr $ concat ["#", n, " ", desc, ": "]
    recoverWith onFail $ do
      action
      safeIO $ putStrLn "PASS"

testSuite :: String -> Spec () -> Spec ()
testSuite name action = do
  safeIO $ putStrLn ""
  safeIO . putStrLn $ "Starting test suite: " ++ name
  action
  n <- getTotal
  f <- getFailed
  safeIO . putStrLn $ concat
    ["Finished running tests. Results: "
    , show (n-f), " passed, "
    , show f    , " failed, "
    , show n    , " total." ]

  when (f > 0) $ fail "The test suite failed to complete successfully."

nextTest :: Spec Int
nextTest = updateTotal (+1) >> getTotal




-- Monad state
-- ---------------------------------------------------------------------

getTotal :: Spec Int
getTotal = stTotal <$> getState

getFailed :: Spec Int
getFailed = stFailed <$> getState

setTotal :: Int -> Spec ()
setTotal x = updateState $ \st -> st { stTotal = x }

setFailed :: Int -> Spec ()
setFailed x = updateState $ \st -> st { stFailed = x }

updateFailed :: (Int -> Int) -> Spec ()
updateFailed f = getFailed >>= setFailed . f

updateTotal :: (Int -> Int) -> Spec ()
updateTotal f = getTotal >>= setTotal . f

