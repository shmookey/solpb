{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Util.Shell where

import Prelude hiding (fail)
import Data.Text (Text, pack, unpack)
import Data.Semigroup ((<>))
import qualified Data.Text as T

import Shelly ((-|-))
import qualified Shelly as Sh

import Control.Monad.Resultant
import Util.ReSpec
import Util

default (T.Text)


shell :: Text -> [Text] -> Maybe Text -> Spec (Int, Text, Text)
shell cmd args input = 
  let
    safely = safeIO . Sh.shelly . Sh.silently . Sh.errExit False
    exec m = do stdout <- m
                stderr <- Sh.lastStderr
                retval <- Sh.lastExitCode
                return (retval, stdout, stderr)
  in safely . exec $
    case input of
      Just stdin -> (return stdin) -|- Sh.run (Sh.fromText cmd) args
      Nothing    -> Sh.run (Sh.fromText cmd) args

safeShell :: Text -> [Text] -> Maybe Text -> Spec Text
safeShell cmd args input = 
  let
    hasError :: Text -> Bool
    hasError = T.isInfixOf "error"
 
    dump :: Text -> Spec Text
    dump txt = do
      dumpfile <- (\n -> cmd <> "-" <> show' n <> ".testdump") <$> getCount
      safeIO $ writeFile (unpack dumpfile) (unpack txt)
      return dumpfile

  in do
    (retval, stdout, stderr) <- shell cmd args input
    if
      retval /= 0 || hasError stdout || hasError stderr
    then case input of
      Just stdin -> dump stdin >>= \dumpfile -> fail . unpack . T.unlines $
          [ "Command failed with exit code " <> show' retval <> ": " <> T.unwords (cmd:args)
          , "Dump of command input saved to: " <> dumpfile
          , "The command generated the following output:"
          , stdin, stderr ]
      Nothing -> fail . unpack . T.unlines $
          [ "Command failed with exit code " <> show' retval <> ": " <> T.unwords (cmd:args)
          , "The command generated the following output:"
          , stdout, stderr ]
    else
      return stdout

