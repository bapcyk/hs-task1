module Lib
    ( diff
    ) where

import System.Process (readProcess)
-- diff :: IO ()
diff rev0 rev1 gitbin = do
  readProcess gitbin ["diff", rev0 ++ ".." ++ rev1] [] >>= putStrLn
