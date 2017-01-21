module Lib
    ( gitdiff
    ) where

import System.Process (readProcess)

-- | Hunk compared files marker, looks like:
-- "diff --git a/app/Main.hs b/app/Main.hs"
data DiffFiles = DiffFiles {
  fileA :: String  -- 1st file path
, fileB :: String  -- 2nd file path
  }

-- | Hunk lines marker, looks like:
-- "@@ -7,11 +7,14 @@ ..."
data DiffRange = DiffRange {
  begA    :: Integer -- fileA hunk start line
, lenA    :: Integer -- file A hunk effected lines number
, begB    :: Integer -- fileB hunk start line
, lenB    :: Integer -- file B hunk effected lines number
  }

-- | Kind of line change: it was added/deleted/unmodified
data ChangeAction = Add | Del | No

-- | Marker of line, looks like:
-- TODO add support of newline
data DiffLine = DiffLine {
  change  :: ChangeAction -- kind of change
, line    :: String       -- line itself
  }

-- | Found diff marker
data Marker =
  NLMarker                -- new-line-at-end-of-file marker
  | LineMarker DiffLine   -- changed/unchanged line marker
  | RangeMarker DiffRange -- range marker
  | FilesMarker DiffFiles -- compared files marker

-- | Get diff between revision `rev0`..`rev1` of local master branch
gitdiff :: String -> String -> String -> IO String
gitdiff rev0 rev1 gitbin = do
  readProcess gitbin ["diff", rev0 ++ ".." ++ rev1] []
