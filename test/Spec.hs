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
import qualified Spec.Proto.ArraysSpec as ArraysSpec
import qualified Spec.Proto.SolTypesSpec as SolTypesSpec


tests :: Spec ()
tests = testSuite "solpb" $ do
  SimpleSpec.tests
  PrimSpec.tests
  ArraysSpec.tests
  SolTypesSpec.tests
  
main :: IO ()
main = testMain tests


