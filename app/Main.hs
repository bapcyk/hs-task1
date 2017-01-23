module Main where

import Lib

import System.Exit
import System.Console.GetOpt
import System.Environment


-- | Command line options
-- XXX stricts to get syntax errors immediately
data CmdOpts = CmdOpts {
  help     :: !Bool   -- show help
, rev0     :: !String -- revision 0
, rev1     :: !String -- revision 1
, badwords :: !String -- bad words
, output   :: !String -- output HTML file
, git      :: !String -- path to Git binary
  } deriving Show


-- | Default values of command line options
defaultCmdOpts :: CmdOpts
defaultCmdOpts = CmdOpts {
  help = False
, rev0 = "HEAD~1"
, rev1 = "HEAD"
, badwords = ""
, output = "output.html"
, git = "git"
  }


-- | Definition of program command line options
cmdSyntax :: [OptDescr (CmdOpts -> CmdOpts)]
cmdSyntax =
  [
    Option ['h', '?'] ["help"]
      (NoArg (\opts -> opts {help=True} ))
      "print this help"

  , Option ['r'] ["rev0"]
      (ReqArg (\a opts -> opts {rev0=a}) "STR")
      "from revision (default: HEAD~1)"

  , Option ['R'] ["rev1"]
      (ReqArg (\a opts -> opts {rev1=a}) "STR")
      "to revision (default: HEAD)"

  , Option ['b'] ["badwords"]
      (ReqArg (\a opts -> opts {badwords=a}) "STR")
      "badwords string"

  , Option ['o'] ["output"]
      (ReqArg (\a opts -> opts {output=a}) "FILE")
      "output file (default: output.html)"

  , Option ['g'] ["git"]
      (ReqArg (\a opts -> opts {git=a}) "PATH")
      "path to Git binary (default: git)"
  ]


-- | Usage string
usage :: String
usage = usageInfo "SYNTAX: task1 [options...]" cmdSyntax


-- | Parses command line options
parseCmdOpts :: IO CmdOpts
parseCmdOpts = do
  argv <- getArgs
  case getOpt Permute cmdSyntax argv of
    (opts, _, []) -> return $ foldl (flip id) defaultCmdOpts opts
    (_, _, errs) -> error $ concat errs ++ "\n" ++ usage


-- | Main entry
main :: IO ()
main = do
  cmdOpts <- parseCmdOpts
  if (help cmdOpts)
    then putStrLn usage
    else if null (badwords cmdOpts)
         then putStrLn "Option `-b' is mandatory\n"
              >> putStr usage
              >> exitFailure
         else do
              gitHtmlDiff (rev0 cmdOpts) (rev1 cmdOpts) (git cmdOpts)
                (words $ badwords cmdOpts)
                >>= writeFile (output cmdOpts)
