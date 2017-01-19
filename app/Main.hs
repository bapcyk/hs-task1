module Main where

import Lib

import System.Console.GetOpt
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as LBS


-- | Command line options
-- XXX stricts to get syntax errors immediately
data CmdOpts = CmdOpts {
  help     :: !Bool -- show help
, query    :: !String -- query string
, output   :: !String -- output JSON file
  } deriving Show


-- | Default values of command line options
defaultCmdOpts :: CmdOpts
defaultCmdOpts = CmdOpts {
  help = False
, query = ""
, output = "output.json"
  }


-- | Definition of program command line options
cmdSyntax :: [OptDescr (CmdOpts -> CmdOpts)]
cmdSyntax =
  [
    Option ['h', '?'] ["help"]
      (NoArg (\opts -> opts {help=True} ))
      "print this help"

  , Option ['q'] ["query"]
      (ReqArg (\a opts -> opts {query=a}) "STR")
      "query string"

  , Option ['o'] ["output"]
      (ReqArg (\a opts -> opts {output=a}) "FILE")
      "output file (default: output.json)"
  ]


-- | Usage string
usage :: String
usage = usageInfo "SYNTAX: task2 [options...]" cmdSyntax


-- | Parses command line options
parseCmdOpts :: IO CmdOpts
parseCmdOpts = do
  argv <- getArgs
  case getOpt Permute cmdSyntax argv of
    (opts, _, []) -> return $ foldl (flip id) defaultCmdOpts opts
    (_, _, errs) -> error $ concat errs ++ "\n" ++ usage


-- | Writes top-10 to file
writeTop10 :: ToJSON a => CmdOpts -> [a] -> IO ()
writeTop10 cmdOpts =
  LBS.writeFile (output cmdOpts) . encode . take 10


-- | Main entry
main :: IO ()
main = do
  cmdOpts <- parseCmdOpts
  let CmdOpts { help = fHelp } = cmdOpts in
    if fHelp then putStrLn usage
    else do
      topLinks (searchUrl (query cmdOpts)) >>= (writeTop10 cmdOpts)
