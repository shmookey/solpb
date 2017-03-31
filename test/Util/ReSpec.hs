module Util.ReSpec where

{- ReSpec: Resultant-based test specifications -}

import Prelude hiding (fail)
import Control.Monad (when)
import System.Exit (exitFailure)

import Control.Monad.Result
import Control.Monad.Resultant


type Spec = ResultantT IO State String

data State = State
  { stFailed      :: Int
  , stSkipped     :: Int
  , stPassed      :: Int
  , stCount       :: Int
  , stSpecName    :: String
  , stExpecting   :: Int
  }

initState :: State
initState = State 0 0 0 0 "(global)" 0

testMain :: Spec () -> IO ()
testMain m = do
  (_, r) <- runResultantT m initState
  case r of
    Ok _ -> return ()
    Err e -> putStrLn e >> exitFailure

spec :: String -> Int -> Spec () -> Spec ()
spec desc num action = 
  let
    onFail :: String -> Spec ()
    onFail err = do
      exp <- getExpecting
      safeIO . putStrLn $ "An error occured in the setup for spec " ++ desc ++ "."
      safeIO . putStrLn $ "The message was: " ++ err
      safeIO . putStrLn $ "The remaining " ++ (show exp) ++ " tests in this spec will be skipped."
      updateSkipped (+exp)
      updateCount (+exp)
  in do
    setSpecName desc
    setExpecting num
    recoverWith onFail action
    exp <- getExpecting
    if exp > 0
    then do
      safeIO . putStr $ "Warning: fewer than expected tests were run for spec " ++ desc
      safeIO . putStrLn $ " (expected " ++ (show num) ++ ", got " ++ show (num - exp) ++ ")"
    else if exp < 0
    then do
      safeIO . putStr $ "Warning: more than expected tests were run for spec " ++ desc
      safeIO . putStrLn $ " (expected " ++ (show num) ++ ", got " ++ show (num - exp) ++ ")"
    else 
      return ()
    setExpecting 0
    

test :: String -> Spec () -> Spec ()
test desc action = 
  let
    onFail :: String -> Spec ()
    onFail err = do
      safeIO $ putStrLn "FAIL"
      safeIO . putStrLn $ "Test failed with message: " ++ err
      updateFailed (+1)
  in do
    n <- show <$> nextTest
    cur <- getSpecName
    safeIO . putStr $ concat ["#", n, " [", cur, "] ", desc, ": "]
    recoverWith onFail $ do
      action
      safeIO $ putStrLn "PASS"
      updatePassed (+1)

testSuite :: String -> Spec () -> Spec ()
testSuite name action = do
  safeIO $ putStrLn ""
  safeIO . putStrLn $ "Starting test suite: " ++ name
  action
  p <- getPassed
  f <- getFailed
  s <- getSkipped
  c <- getCount
  safeIO . putStrLn $ concat
    ["Finished running tests. Results: "
    , show p , " passed, "
    , show f , " failed, "
    , show s , " skipped, "
    , show c , " total." ]

  when (p < c) $ fail "The test suite failed to complete successfully."

nextTest :: Spec Int
nextTest = do
  updateCount (+1)
  updateExpecting (\x -> x-1)
  getCount


-- Monad state
-- ---------------------------------------------------------------------

getPassed     = stPassed <$> getState
getFailed     = stFailed <$> getState
getSkipped    = stSkipped <$> getState
getCount      = stCount <$> getState
getSpecName   = stSpecName <$> getState
getExpecting  = stExpecting <$> getState

setPassed x  = updateState $ \st -> st { stPassed = x }
setFailed x  = updateState $ \st -> st { stFailed = x }
setSkipped x = updateState $ \st -> st { stSkipped = x }
setCount x   = updateState $ \st -> st { stCount = x }
setSpecName x = updateState $ \st -> st { stSpecName = x }
setExpecting x = updateState $ \st -> st { stExpecting = x }


updatePassed f = getPassed >>= setPassed . f
updateFailed f = getFailed >>= setFailed . f
updateSkipped f = getSkipped >>= setSkipped . f
updateCount f = getCount >>= setCount . f
updateExpecting f = getExpecting >>= setExpecting . f

