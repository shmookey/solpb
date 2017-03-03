module Main where

import qualified Options.Applicative as Opts
import qualified Data.ByteString.Lazy as B
import Options.Applicative ((<>))
import qualified Text.ProtocolBuffers.ProtoCompile.Parser as Parser
import qualified Text.DescriptorProtos.FileDescriptorProto as Proto
import System.Directory (createDirectory)
import Convert
import Data.List (intercalate)

data Options = Options
  { optInput :: String
  , optDir   :: FilePath
  } deriving (Show)


readCliOpts :: IO Options
readCliOpts =
  Opts.execParser $ Opts.info (Opts.helper <*> cliOpts)
    ( Opts.fullDesc
   <> Opts.header   "solpb -- protocol buffers for solidity" 
   <> Opts.progDesc "Generate solidity libraries for working with protocol buffers." )
  where 
    cliOpts = Options
      <$> Opts.argument Opts.str
          ( Opts.metavar "FILE"
         <> Opts.help    "Path to input file." )
      <*> Opts.strOption
          ( Opts.long    "out"
         <> Opts.short   'o'
         <> Opts.value   "."
         <> Opts.help    "Output directory." )

processDescriptor :: FilePath -> Proto.FileDescriptorProto -> IO ()
processDescriptor outDir file =
  let
    writeSrc (k, v) = writeFile (outDir ++ "/" ++ k ++ ".sol") v
  in
    mapM_ writeSrc $ Convert.convert file

main :: IO ()
main = readCliOpts >>= \o ->
  let
    input  = optInput o
    outDir = optDir o
  in do
    result <- Parser.parseProto input <$> B.readFile input

    if not (outDir == ".")
    then createDirectory outDir
    else return ()

    case result of
      Left e  -> putStrLn $ show e
      Right x -> processDescriptor outDir x

