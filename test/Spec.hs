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

import Gen.Prim.Prim

import Util.ReSpec
import qualified Spec.Proto.SimpleSpec as SimpleSpec
import qualified Spec.Proto.PrimSpec as PrimSpec


tests :: Spec ()
tests = testSuite "solpb" $ do
  SimpleSpec.test
  PrimSpec.test
  
main :: IO ()
main = testMain tests


