{-# LANGUAGE OverloadedStrings #-}

module Util.Solidity where

import Prelude hiding (fail)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text, pack, unpack)
import System.Directory
import Text.Printf (printf)
import Data.Bits (Bits, (.|.), complement, shiftL, shiftR)
import Data.Bits.ByteString
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
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

-- todo: check for out of gas condition

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

padBytes :: Int -> ByteString -> ByteString
padBytes i x =
  if B8.length x == i
  then
    x
  else
    B8.cons '\NUL' (padBytes (i-1) x)
--  else
--    let
--      sz  = B8.length x
--      n   = i - (sz `mod` i)
--      pad = fst . B16.decode . B8.pack . take (n*2) $ repeat '0'
--    in
--      B8.append pad x

roll :: ByteString -> Integer
roll = B.foldl' unstep 0
  where unstep a b = a `shiftL` 8 .|. fromIntegral b

unroll :: (Integral a, Bits a) => a -> ByteString
unroll = B.reverse . B.unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

-- | Fixed-preicision version of `unroll`
funroll :: Int -> Integer -> ByteString
funroll sz = padBytes sz . unroll

-- | Signed, fixed-precision version of `unroll`
sunroll :: Int -> Integer -> ByteString
sunroll sz x = 
  if 
    x >= 0
  then
    padBytes sz $ unroll x
  else
    complement . padBytes sz . unroll $ abs x - 1

fromHex :: String -> B8.ByteString
fromHex = fst . B16.decode . B8.pack

--fromHex :: String -> B8.ByteString
--fromHex = fst . B16.decode . B8.pack

-- Lazy Bytestring versions
-- ---------------------------------------------------------------------

lfunroll :: Int -> Integer -> BL8.ByteString
lfunroll sz = BL8.fromStrict . funroll sz

lsunroll :: Int -> Integer -> BL8.ByteString
lsunroll sz = BL8.fromStrict . sunroll sz

lfromHex :: String -> BL8.ByteString
lfromHex = BL8.fromStrict . fromHex

