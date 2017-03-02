module Main where

import qualified Options.Applicative as Opts
import qualified Data.ByteString.Lazy as B
import Options.Applicative ((<>))
import qualified Text.ProtocolBuffers.ProtoCompile.Parser as Parser
import qualified Text.DescriptorProtos.FileDescriptorProto as Proto
import Convert
import Data.List (intercalate)

data Options = Options
  { optInput :: String
  , optDir   :: String
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
          ( Opts.long    "dir"
         <> Opts.short   'd'
         <> Opts.help    "Output directory." )

processDescriptor :: Proto.FileDescriptorProto -> String
processDescriptor =
  intercalate "\n" . Convert.convert

main :: IO ()
main = readCliOpts >>= \o ->
  let
    input = optInput o
  in do
    result <- Parser.parseProto input <$> B.readFile input
    putStrLn $ case result of
      Left e  -> show e
      Right x -> processDescriptor x

