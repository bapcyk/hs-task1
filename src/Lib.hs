module Lib where

import System.Process (readProcess)

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

instance Read DiffNL where
  readsPrec _ "\\No new line at end of file" = [(DiffNL, "")]
  readsPrec _ _ = []

-- identMarker :: String -> Marker
-- identMarker s =
--   if s == "\\ No newline at end of file" then
--     NLMarker
--     else if "@@"

-- | Get diff between revision `rev0`..`rev1` of local master branch
gitdiff :: String -> String -> String -> IO String
gitdiff rev0 rev1 gitbin = do
  readProcess gitbin ["diff", rev0 ++ ".." ++ rev1] []
