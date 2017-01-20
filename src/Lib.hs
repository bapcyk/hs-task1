module Lib
    ( gitdiff
    ) where

import System.Process (readProcess)

-- | Location of found word
data WordLoc = WordLoc {
  file  :: String  -- file where word was found
, nline :: Integer -- line number where word was found
  }

-- | Get diff between revision `rev0`..`rev1` of local master branch
gitdiff :: String -> String -> String -> IO String
gitdiff rev0 rev1 gitbin = do
  readProcess gitbin ["diff", rev0 ++ ".." ++ rev1] []
