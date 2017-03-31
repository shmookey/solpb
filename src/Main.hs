{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Extra (unlessM)
import Options.Applicative ((<>))
import Data.Text (unpack)
import Data.List (nub)
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
    addDefaults o = o
      { optIncludeDirs = nub (".":(optIncludeDirs o))
      }
    cliOpts = addDefaults <$> (Options
      <$> Opts.strOption
          ( Opts.long    "out"
         <> Opts.metavar "DIR"
         <> Opts.short   'o'
         <> Opts.value   "."
         <> Opts.help    "Output directory. Default: ." )
      <*> Opts.strOption
          ( Opts.long    "lib-name"
         <> Opts.metavar "NAME"
         <> Opts.short   'l'
         <> Opts.value   "pb"
         <> Opts.help    "Name of package library to generate. Default: pb" )
      <*> Opts.many (Opts.strOption
          ( Opts.long    "include"
         <> Opts.metavar "DIR"
         <> Opts.short   'I'
         <> Opts.help    "Add a directory to the search path" ))
      <*> Opts.switch
          ( Opts.long    "separate"
         <> Opts.short   's'
         <> Opts.help    "Generate separate output files for each specified input.")
      <*> Opts.strOption
          ( Opts.long    "suffix"
         <> Opts.metavar "NAME"
         <> Opts.short   'S'
         <> Opts.value   "_pb"
         <> Opts.help    "Output filename suffix, used with --separate. Default: _pb" )
      <*> Opts.switch
          ( Opts.long    "no-pragma"
         <> Opts.help    "Suppress solidity version pragma in output" )
      <*> Opts.switch
          ( Opts.long    "no-runtime"
         <> Opts.help    "Do not append runtime library to output files, implied by --separate" )
      <*> Opts.switch
          ( Opts.long    "no-combine"
         <> Opts.help    "Do not produce a combined package library" )
      <*> Opts.optional (Opts.strOption
          ( Opts.long    "runtime-out"
         <> Opts.help    "Path relative to output dir to save runtime lib, ignores --no-runtime" ))
      <*> Opts.many (Opts.argument Opts.str
          ( Opts.metavar "[FILES...]"
         <> Opts.help    "Path to input file(s)" )))

app :: App ()
app = readCliOpts >>= \o ->
  let
    inputFiles   = optInputs o
    outputDir    = optDir o
    outputSuffix = optSuffix o
    libName      = optLibName o

    outputTarget :: String -> FilePath
    outputTarget baseName = FilePath.combine outputDir fullName
      where fullName = baseName ++ outputSuffix ++ ".sol"

    processFile :: FilePath -> App ()
    processFile path = 
      let
        name   = FilePath.takeBaseName path
        target = outputTarget name
      in do
        structs <- Convert.load path
        code    <- Generator.generate structs
        lift $ writeFile target (unpack code)

    processAllFiles :: [FilePath] -> App ()
    processAllFiles paths = 
      let
        target = FilePath.combine outputDir (libName ++ ".sol")
      in do
        structs <- Convert.loadMany paths
        code    <- Generator.generate structs
        lift $ writeFile target (unpack code)

  in do
    setConfig o
    lift $ ensureDirectory outputDir

    if
      optSeparate o
    then
      mapM_ processFile inputFiles
    else
      processAllFiles inputFiles

    case optRuntimeOut o of
      Just path -> safeIO $ 
        writeFile (FilePath.combine outputDir path) 
                  (unpack Generator.generateRuntimeLibrary)
      Nothing -> return ()

main :: IO ()
main = do
  (_, r) <- runResultantT app initState
  case r of
    Ok xs -> return ()
    Err e -> putStrLn $ "Error: " ++ e


