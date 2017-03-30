{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Extra (unlessM)
import Options.Applicative ((<>))
import Data.Text (unpack)
import qualified Text.ProtocolBuffers.ProtoCompile.Parser as Parser
import qualified Options.Applicative as Opts
import qualified Data.ByteString.Lazy as B
import qualified System.FilePath as FilePath

import Control.Monad.Result
import Control.Monad.Resultant
import Convert
import Generator
import Types
import Util (ensureDirectory)
import qualified Build

readCliOpts :: App Options
readCliOpts =
  lift . Opts.execParser $ Opts.info (Opts.helper <*> cliOpts)
    ( Opts.fullDesc
   <> Opts.header   "solpb -- protocol buffers for solidity" 
   <> Opts.progDesc "Generate solidity libraries for working with protocol buffers." )
  where 
    cliOpts = Options
      <$> Opts.strOption
          ( Opts.long    "out"
         <> Opts.short   'o'
         <> Opts.value   "."
         <> Opts.help    "Output directory" )
      <*> Opts.strOption
          ( Opts.long    "suffix"
         <> Opts.short   's'
         <> Opts.value   "_pb"
         <> Opts.help    "Output file name suffix" )
      <*> Opts.switch
          ( Opts.long    "pragma"
         <> Opts.short   'p'
         <> Opts.help    "Emit solidity version pragmas in generated code" )
      <*> Opts.some (Opts.argument Opts.str
          ( Opts.metavar "FILE [FILES...]"
         <> Opts.help    "Path to input file(s)" ))

app :: App ()
app = readCliOpts >>= \o ->
  let
    inputFiles   = optInputs o
    outputDir    = optDir o
    outputSuffix = optSuffix o

    outputTarget :: String -> FilePath
    outputTarget baseName = FilePath.combine outputDir fullName
      where fullName = baseName ++ outputSuffix ++ ".sol"

    processFile :: FilePath -> App ()
    processFile path = 
      let
        name   = FilePath.takeBaseName path
        target = outputTarget name
      in do
        txt     <- lift $ B.readFile path
        protos  <- mapEither show $ Parser.parseProto name txt
        structs <- Convert.collect protos
        code    <- Generator.generate structs
        lift $ writeFile target (unpack code)

  in do
    lift $ ensureDirectory outputDir
    mapM_ processFile inputFiles

main :: IO ()
main = do
  (_, r) <- runResultantT app ()
  case r of
    Ok xs -> return ()
    Err e -> putStrLn $ "Error: " ++ e


