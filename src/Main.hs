module Main where

import qualified Options.Applicative as Opts
import qualified Data.ByteString.Lazy as B
import Options.Applicative ((<>))
import qualified Text.ProtocolBuffers.ProtoCompile.Parser as Parser
import qualified Text.DescriptorProtos.FileDescriptorProto as Proto
import System.Directory (createDirectory, doesDirectoryExist)
import Convert
import Data.List (intercalate)

data Options = Options
  { optDir    :: FilePath
  , optSuffix :: String
  , optPragma :: Bool
  , optInputs :: [FilePath]
  } deriving (Show)


readCliOpts :: IO Options
readCliOpts =
  Opts.execParser $ Opts.info (Opts.helper <*> cliOpts)
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

processDescriptor :: Bool -> FilePath -> String -> Proto.FileDescriptorProto -> IO ()
processDescriptor pragma outDir suffix file =
  let
    header          = if pragma then "pragma solidity ^0.4.1;\n\n" else ""
    writeSrc (k, v) = writeFile (outDir ++ "/" ++ k ++ suffix ++ ".sol") (header ++ v)
  in
    mapM_ writeSrc $ Convert.convert file

main :: IO ()
main = readCliOpts >>= \o ->
  let
    inputs = optInputs o
    outDir = optDir o
    suffix = optSuffix o
    pragma = optPragma o

    procResult r = case r of
      Left e  -> putStrLn $ show e
      Right x -> processDescriptor pragma outDir suffix x
      
  in do
    results <- mapM (\x -> Parser.parseProto x <$> B.readFile x) inputs

    dirExists <- doesDirectoryExist outDir
    if not dirExists
    then createDirectory outDir
    else return ()

    mapM_ procResult results

