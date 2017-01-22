--{-# LANGUAGE RankNTypes #-}
module Lib where


import Data.Char
import Data.Bool (bool)
import Control.Applicative ((<|>))
import Control.Monad (guard,mplus)
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Text.Read (readMaybe)
import System.Process (readProcess)
import Data.List (isPrefixOf, intercalate)


--------------------------------- Types ---------------------------------------

-- | Hunk compared files marker, looks like:
-- "diff --git a/app/Main.hs b/app/Main.hs"
data DiffFiles = DiffFiles {
  fileA :: String  -- 1st file path
  ,fileB :: String  -- 2nd file path
  } deriving Show


-- | Hunk lines marker, looks like:
-- "@@ -7,11 +7,14 @@ ..."
data DiffRange = DiffRange {
  begA    :: Integer -- fileA hunk start line
  ,lenA    :: Integer -- file A hunk effected lines number
  ,begB    :: Integer -- fileB hunk start line
  ,lenB    :: Integer -- file B hunk effected lines number
  } deriving Show


-- | Kind of line change: it was added/deleted/unmodified
data ChangeAction = Add | Del | No
  deriving Show


-- | Marker of line, looks like:
-- "+ ..." or "- ..." or "..."
data DiffLine = DiffLine {
  change  :: ChangeAction -- kind of change
  ,line    :: String       -- line itself
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


-- | Highlight string (usual or highlighted)
data HiStr = LoStr String|HiStr String
  deriving Show


-- | Output string is list of possible highlighted strings
type OutStr = [HiStr]


-- | Output items (to pass into html)
data OutItem =
  OutFiles DiffFiles                    -- comparing files
  | OutRange DiffRange                  -- hunk range
  | OutLine ChangeAction OutStr Integer -- (change, list-of-hi-words, hunk-line-num)
  deriving Show


-- | Context of iteration over diff lines
data Ctx = Ctx {
  files :: DiffFiles     -- comparing files
  ,range :: DiffRange     -- hunk range
  ,hunkLineNum :: Integer -- line number in the hunk
  ,outItems :: [OutItem]  -- output items
  ,badWords :: [String]   -- "bad" words (to be highlighted)
  } deriving Show


-- | Class for convertable to HTML instances
class ShowHtml a where
  showHtml :: a -> String


----------------------------- Parse utilities ---------------------------------

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
readParsedWith :: MaybeT (State String) t -> String -> [(t,String)]
readParsedWith parsefn s =
  case runState (runMaybeT parsefn) s of
    (Nothing,_) -> []
    (Just some,_) -> [(some,"")]


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
      Just some -> [(some,"")]
    where
        marker = foldl1 (<|>) [FilesMarker <$> (readMaybe s :: Maybe DiffFiles),
                               RangeMarker <$> (readMaybe s :: Maybe DiffRange),
                               LineMarker  <$> (readMaybe s :: Maybe DiffLine)]


--------------------------- Search words utilities ----------------------------

-- | Highlights words `ws` in input string `s`, produces [HiStr x/LoStr x, ...]
hiWords :: [String] -> String -> OutStr
hiWords ws s =
  map (\e -> (bool LoStr HiStr (elem e ws)) e) $ words s


---------------------------- Process input utilities --------------------------

-- | Creates empty context w/ bad words `bw`
emptyCtx :: [String] -> Ctx
emptyCtx bw = Ctx (DiffFiles "" "") (DiffRange 0 0 0 0) 0 [] bw


-- | Processes one input line from diff output
procDiffLine :: Ctx -> String -> Ctx
procDiffLine ctx s =
  case readMaybe s :: Maybe Marker of
    Nothing -> ctx
    Just (LineMarker l) -> ctx {
      -- TODO 0 - linenum in hunk
      outItems=(outItems ctx)++[OutLine (change l) (hiWords (badWords ctx) (line l)) 0]
      }
    Just (RangeMarker r) -> ctx {
      range=r
      ,outItems=(outItems ctx)++[OutRange r]} -- time for lense ;)
    Just (FilesMarker f) -> ctx {
      files=f
      ,outItems=(outItems ctx)++[OutFiles f]}


------------------------------ HTML output utilities --------------------------

-- | Escapes string `s` to be HTML safe
escHtml :: String -> String
escHtml s = intercalate "" $ map repl s
  where
    repl '\n' = "<br/>"
    repl '\t' = "&emsp;"
    repl ' '  = "&nbsp;"
    repl '<'  = "&lt;"
    repl '>'  = "&gt;"
    repl '&'  = "&amp;"
    repl c    = [c]


-- | Show OutItem as HTML
instance ShowHtml OutItem where
  showHtml (OutFiles f) =
    ("<div class=\"filea\"><b>FILE A: </b>"++(fileA f)++"</div>"
    ++ "<div class=\"fileb\"><b>FILE B: </b>"++(fileB f)++"</div>")
  showHtml (OutRange r) =
    ("<div class=\"range\"><b>CHANGE OF FILE A - from-to: </b>"++(show $ begA r)
    ++"-"++(show $ begA r + lenA r - 1)++"</div>"
    ++"<div class=\"range\"><b>CHANGE OF FILE B - from-to: </b>"++(show $ begB r)
    ++"-"++(show $ begB r + lenB r - 1)++"</div>")
  showHtml (OutLine ca os ln) =
    "<div class=\"line "++(cls ca)++"\">"++(intercalate " " $ map hi os)++"</div>"
    where hi (HiStr s) = "<span class=\"highlight\">"++(escHtml s)++"</span>"
          hi (LoStr s) = escHtml s
          cls Add = "added"
          cls Del = "deleted"
          cls No  = "unmodified"


-- | Show Ctx as HTML
instance ShowHtml Ctx where
  showHtml ctx = foldl1 (++) $ map showHtml (outItems ctx)


-- | HTML header
htmlHeader rev0 rev1 =
  ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"HTTP://WWW.W3.ORG/TR/REC-HTML40/STRICT.DTD\">"
   ++"<html><head>"
   ++"<meta http-equiv=\"content-type\" content=\"text/html; charset=ISO-8859-1\">"
   ++"<link rel=\"stylesheet\" href=\"task2.css\" type=\"text/css\" media=\"all\">"
   ++"<title>Diff between revisions "++rev0++".."++rev1++"</title>"
   ++"</head><body>")


-- | HTML footer
htmlFooter = "</body></html>"


---------------------------------- Git utilities ------------------------------

-- | Get diff between revision `rev0`..`rev1` of local master branch
gitdiff :: String -> String -> String -> IO String
gitdiff rev0 rev1 gitbin = do
  readProcess gitbin ["diff",rev0++".."++rev1] []


-- | Processes diff output w/ bad words list `bw`
procDiff :: [String] -> String -> Ctx
procDiff bw = foldl procDiffLine (emptyCtx bw) . lines


-- | The same as gitdiff but returns HTML as IO String; `bw` is list of bad
-- words
gitHtmlDiff :: String -> String -> String -> [String] -> IO String
gitHtmlDiff rev0 rev1 gitbin bw =
  gitdiff rev0 rev1 gitbin >>= return . procDiff bw >>= return . asHtml
  where asHtml ctx = (htmlHeader rev0 rev1)++showHtml ctx++htmlFooter
