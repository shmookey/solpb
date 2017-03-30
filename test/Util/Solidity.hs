{-# LANGUAGE OverloadedStrings #-}

module Util.Solidity where

import Prelude hiding (fail)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text, pack, unpack)
import System.Directory
import Text.Printf (printf)
import Data.Bits (Bits, (.|.), shiftL, shiftR)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Text as T

import Crypto.Hash (Digest, Keccak_256, hash)

import Control.Monad.Resultant
import Util.Shell
import Util.ReSpec
import Util (ensureDirectory)
import Types


tmpContractDir = "test/Gen/solidity"
tmpBytecodeDir = "test/Gen/bytecode"

compile :: Text -> Code -> Spec FilePath
compile name code = 
  let
    heading = T.concat ["======= <stdin>:", name, " ======="]
    solTemp = tmpContractDir ++ "/" ++ (unpack name) ++ ".sol"
    evmTemp = tmpBytecodeDir ++ "/" ++ (unpack name) ++ ".bin"
  in do
    safeIO $ ensureDirectory tmpContractDir
    safeIO $ ensureDirectory tmpBytecodeDir

    safeIO $ writeFile solTemp (unpack code)
    output <- safeShell "solc" ["--bin-runtime"] (Just code)
    let bytecode = (T.lines . snd $ T.breakOn heading output) !! 2
    safeIO $ writeFile evmTemp (unpack bytecode)
    return evmTemp

runEVM :: FilePath -> Text -> Spec Text
runEVM inputFile callData =
  safeShell "evm" ["--codefile", pack inputFile, "--input", callData, "run"] Nothing

callDataWithBytes :: Text -> Text -> Text
callDataWithBytes name bs =
  let
    method = methodID name ["bytes"]
    loc    = padWordL "20"
    len    = pack . printf "%064x" $ (T.length bs `div` 2)
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
    T.take 8 . keccak256 . B8.pack $ unpack sig

extractReturnedBytes :: Text -> Text
extractReturnedBytes returnData =
  let
    bs = fst . B16.decode . B8.pack . drop 2 $ unpack returnData
    (sizePart, dataPart) = B8.splitAt 32 $ B8.drop 32 bs
    size = fromInteger $ roll sizePart
  in
    T.pack . B8.unpack . B16.encode $ B8.take size dataPart

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

roll :: ByteString -> Integer
roll = B.foldl' unstep 0
  where unstep a b = a `shiftL` 8 .|. fromIntegral b

unroll :: (Integral a, Bits a) => a -> ByteString
unroll = B.reverse . B.unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

