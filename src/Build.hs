{-# LANGUAGE TemplateHaskell #-}

module Build where


{- | Compile-time template to bake the protobuf runtime library into solpb -}

import Language.Haskell.TH


pbLibrary :: Q Exp
pbLibrary = runIO $ do
  xs <- readFile "lib/solidity/libsolpb.sol"
  return . LitE $ StringL xs

