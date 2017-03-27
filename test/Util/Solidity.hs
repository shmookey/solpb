{-# LANGUAGE OverloadedStrings #-}

module Util.Solidity where

import Prelude hiding (fail)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text, pack, unpack)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import Crypto.Hash (Digest, Keccak_256, hash)
import Shelly ((-|-))
import qualified Shelly as Sh

import Control.Monad.Resultant
import Types


silent = safeIO 
       . Sh.shelly 
       . Sh.silently
       . Sh.errExit False 

compile :: Text -> Code -> App Text
compile name code = 
  let
    onFailure :: String -> App Text
    onFailure err =
      let x = unpack name
      in do
        lift . putStrLn $ "Error compiling contract " ++ x
        lift . putStrLn $ "The command output was: " ++ err
        lift $ writeFile (x ++ "-dump.sol") (unpack code)
        lift . putStrLn $ "Contract code dumped to: " ++ x ++ "-dump.sol"
        -- lift . putStrLn $ "Command output: " ++ output
        fail $ "Error compiling contract: " ++ err

    heading = T.concat ["======= <stdin>:", name, " ======="]

  in recoverWith onFailure $ do
    (ret, out) <- silent
      $ do out <- (return code) -|- Sh.run "solc" ["--bin-runtime"]
           ret <- Sh.lastExitCode
           err <- Sh.lastStderr
           return (ret, T.append out err)
    case ret of
      0 -> return . (!! 2) . T.lines . snd $ T.breakOn heading out
      _ -> fail $ unpack out

run :: Text -> Text -> App Text
run code input = do
  result <- silent $ Sh.run "evm" ["--code", code, "--input", input, "run"]
  if T.isInfixOf "error" result
  then fail . show $ T.append "Error running EVM: " result
  else return result

callDataWithBytes :: Text -> Text -> Text
callDataWithBytes name bs =
  let
    method = methodID name ["bytes"]
    loc    = padWordL "20"
    len    = padWordL . pack $ show ((T.length bs) `div` 2)
  in
    T.concat [method, loc, len, padWordR bs]

callDataNoArgs :: Text -> Text
callDataNoArgs name =
  methodID name []

methodID :: Text -> [Text] -> Text
methodID name args =
  let
    sig = T.concat [name, "(", T.intercalate "," args, ")"]
  in
    T.take 8 . keccak256 . B.pack $ unpack sig

keccak256 :: ByteString -> Text
keccak256 bs = pack $ show (hash bs :: Digest Keccak_256)

padWordL :: Text -> Text
padWordL x = 
  let
    sz  = T.length x
    n   = 64 - (sz `mod` 64)
    pad = pack . take n $ repeat '0'
  in
    T.append pad x

padWordR :: Text -> Text
padWordR x = 
  let
    sz  = T.length x
    n   = 64 - (sz `mod` 64)
    pad = pack . take n $ repeat '0'
  in
    T.append x pad

