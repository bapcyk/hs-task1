module Lib
    ( diff
    ) where

import System.Process (readProcess)

-- | Location of found word
data WordLoc = WordLoc {
  file  :: String  -- file where word was found
, nline :: Integer -- line number where word was found
  }

-- diff :: IO ()
diff rev0 rev1 gitbin = do
  readProcess gitbin ["diff", rev0 ++ ".." ++ rev1] [] >>= putStrLn
