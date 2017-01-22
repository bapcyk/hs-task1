--{-# LANGUAGE RankNTypes #-}
module Lib where


import Data.Char
import Control.Applicative ((<|>))
import Control.Monad (guard, mplus)
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Text.Read (readMaybe)
import System.Process (readProcess)
import Data.List (isPrefixOf)


--------------------------------- Types ---------------------------------------

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


-- | Other diff lines like messages, etc - to be ignored (not a text)
-- data DiffEtc= DiffEtc
--   deriving Show


-- | Found diff marker
data Marker =
  -- EtcMarker DiffEtc       -- ignoring diff lines
    LineMarker DiffLine   -- changed/unchanged line marker
  | RangeMarker DiffRange -- range marker
  | FilesMarker DiffFiles -- compared files marker
  deriving Show


----------------------------- Common utilities --------------------------------

-- | "Reads" some items (chars) until `end` returns True. Returns readed items
-- and tail (not readed) because it uses state
readRawUntil :: (a -> Bool) -> State [a] [a]
readRawUntil end = do
  s <- get
  let taken = takeWhile (not . end) s
  put $ drop (length taken) s
  return taken


-- | Reader of DiffEtc diff's strings
-- instance Read DiffEtc where
--   readsPrec _ "\\No new line at end of file" = [(DiffEtc, "")]
--   readsPrec _ _ = []


-- | Reads raw string until `unt` returns True. Readed value is compared w/
-- expected raw string `exp` and if is not matched, calls `guard` in current monad
-- context. `lift` is needed to make function of internal monad (State) to work
-- w/ data of external monad (MaybeT) due to usage w/ `>>=` of external monad
_isExpectedBefore exp unt = lift (readRawUntil unt) >>= guard . (exp==)


-- | Reads string as Integer
atoi = read :: String->Integer


-- | Parses DiffRange string, returns (range::Maybe DiffRange, notParsed::String)
parseDiffRange :: MaybeT (State String) DiffRange
parseDiffRange = do
  "@@ -" `_isExpectedBefore` isNumber
  begA <- lift (readRawUntil isPunctuation)
  "," `_isExpectedBefore` isNumber
  lenA <- lift (readRawUntil isSpace)
  " +" `_isExpectedBefore` isNumber
  begB <- lift (readRawUntil isPunctuation)
  "," `_isExpectedBefore` isNumber
  lenB <- lift (readRawUntil isSpace)
  return $ DiffRange (atoi begA) (atoi lenA) (atoi begB) (atoi lenB)


-- | Implementation of generci `readsPrec` where input string `s` is parsed w/
-- `parsefn` function
readParsedWith :: MaybeT (State String) t -> String -> [(t, String)]
readParsedWith parsefn s =
  case runState (runMaybeT parsefn) s of
    (Nothing, _) -> []
    (Just some, _) -> [(some, "")]


-- | Parses DiffFiles string, returns (files::Maybe DiffFiles, notParsed::String)
parseDiffFiles :: MaybeT (State String) DiffFiles
parseDiffFiles = do
  "diff --git a" `_isExpectedBefore` (=='/')
  fileA <- lift (readRawUntil isSpace)
  " b" `_isExpectedBefore` (=='/')
  fileB <- lift (readRawUntil isSpace)
  -- TODO remove "/" in front of fileX
  return $ DiffFiles fileA fileB


-- | Parses DiffLine, returns (line::Maybe DiffLine, notParsed::String)
parseDiffLine :: MaybeT (State String) DiffLine
parseDiffLine = do
  line <- lift (readRawUntil isControl)
  guard (not (isPrefixOf "+++ " line || isPrefixOf "--- " line))
  guard (null line || isPrefixOf " " line || isPrefixOf "+" line || isPrefixOf "-" line)
  return $ case line of
             '+':t -> DiffLine Add t
             '-':t -> DiffLine Del t
             ' ':t -> DiffLine No t
             _     -> DiffLine No line


-------------------------------- Read instances -------------------------------

-- | Read of DiffRange. Can be used as:
--     read "@@ -1,11 +1,16 @@"::DiffRange
-- as:
--     import Text.Read (readMaybe)
--     readMaybe "@@ -1,11 +1,16 @@"::Maybe DiffRange
instance Read DiffRange where
  readsPrec _ = readParsedWith parseDiffRange


-- | Read of DiffFiles
instance Read DiffFiles where
  readsPrec _ = readParsedWith parseDiffFiles


-- | Read of DiffLine
instance Read DiffLine where
  readsPrec _ = readParsedWith parseDiffLine


-- | Read of any known Marker. Usage: read "diff --git a/xxx b/yyy"::Marker
instance Read Marker where
  readsPrec _ s =
    case marker of
      Nothing -> []
      Just some -> [(some, "")]
    where
        marker = foldl1 (<|>) [FilesMarker <$> (readMaybe s :: Maybe DiffFiles),
                               RangeMarker <$> (readMaybe s :: Maybe DiffRange),
                               LineMarker  <$> (readMaybe s :: Maybe DiffLine)]


-- | Get diff between revision `rev0`..`rev1` of local master branch
gitdiff :: String -> String -> String -> IO String
gitdiff rev0 rev1 gitbin = do
  readProcess gitbin ["diff", rev0 ++ ".." ++ rev1] []
