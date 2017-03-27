{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

import Control.Monad.Result
import Control.Monad.Resultant
import Types
import Util.Solidity
import Util.TestGen
import Util.Protobuf

import qualified Spec.Proto.SimpleSpec as SimpleSpec


runTests :: App ()
runTests = do
  safeIO $ putStrLn "\nStarting tests..."
  SimpleSpec.test

main :: IO ()
main = do
  (_, r) <- runResultantT runTests ()
  case r of
    Ok _ -> return ()
    Err e -> putStrLn $ "Error: " ++ e

