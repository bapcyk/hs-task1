module Lib where


import Data.Char
import Control.Monad (guard)
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Text.Read (readMaybe)
import System.Process (readProcess)
import Data.List (isPrefixOf)


-- | Hunk compared files marker, looks like:
-- "diff --git a/app/Main.hs b/app/Main.hs"
data DiffFiles = DiffFiles {
  fileA :: String  -- 1st file path
, fileB :: String  -- 2nd file path
  } deriving Show


-- | Hunk lines marker, looks like:
-- "@@ -7,11 +7,14 @@ ..."
data DiffRange = DiffRange {
  begA    :: Integer -- fileA hunk start line
, lenA    :: Integer -- file A hunk effected lines number
, begB    :: Integer -- fileB hunk start line
, lenB    :: Integer -- file B hunk effected lines number
  } deriving Show


-- | Kind of line change: it was added/deleted/unmodified
data ChangeAction = Add | Del | No
  deriving Show


-- | Marker of line, looks like:
-- "+ ..." or "- ..." or "..."
data DiffLine = DiffLine {
  change  :: ChangeAction -- kind of change
, line    :: String       -- line itself
  } deriving Show


data DiffNL = DiffNL
  deriving Show


-- | Found diff marker
data Marker =
  NLMarker DiffNL         -- new-line-at-end-of-file marker
  | LineMarker DiffLine   -- changed/unchanged line marker
  | RangeMarker DiffRange -- range marker
  | FilesMarker DiffFiles -- compared files marker
  deriving Show


-- | "Reads" some items (chars) until `end` returns True. Returns readed items
-- and tail (not readed) because it uses state
readRawUntil :: (a -> Bool) -> State [a] [a]
readRawUntil end = do
  s <- get
  let taken = takeWhile (not . end) s
  put $ drop (length taken) s
  return taken


-- | Reader of DiffNL diff's string
instance Read DiffNL where
  readsPrec _ "\\No new line at end of file" = [(DiffNL, "")]
  readsPrec _ _ = []


-- | Parses DiffRange string, returns (range::Maybe DiffRange, notParsed::String)
parseDiffRange :: MaybeT (State String) DiffRange
parseDiffRange = do
  -- Line:
  --   lift (readRawUntil isNumber) >>= guard . ("@@ -"==)
  -- is the same as:
  --   pre <- lift (readRawUntil isSpace)
  --   guard (pre == "@@ -")
  let isExpectedBefore exp unt = lift (readRawUntil unt) >>= guard . (exp==)
  let int = read :: String->Integer
  "@@ -" `isExpectedBefore` isNumber
  begA <- lift (readRawUntil isPunctuation)
  "," `isExpectedBefore` isNumber
  lenA <- lift (readRawUntil isSpace)
  " +" `isExpectedBefore` isNumber
  begB <- lift (readRawUntil isPunctuation)
  "," `isExpectedBefore` isNumber
  lenB <- lift (readRawUntil isSpace)
  return $ DiffRange (int begA) (int lenA) (int begB) (int lenB)


instance Read DiffRange where
  readsPrec _ s =
    case runState (runMaybeT parseDiffRange) s of
      (Nothing, _) -> []
      (Just range, _) -> [(range, "")]


-- | Get diff between revision `rev0`..`rev1` of local master branch
gitdiff :: String -> String -> String -> IO String
gitdiff rev0 rev1 gitbin = do
  readProcess gitbin ["diff", rev0 ++ ".." ++ rev1] []
