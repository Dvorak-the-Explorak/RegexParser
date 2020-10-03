{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
import Parsing
import Data.Char (chr, ord)  
import Data.List (isInfixOf)
import Data.Text (splitOn, pack, unpack)
import Control.Monad 
import System.Environment
import System.Directory
import Debug.Trace

type Regex = Parser String


-- main = interact $
--   solve . lines

main =  dirmain2 

testmain = do
  regstr <- getLine
  text <- getLine
  let parsers = parse regex regstr
  putStrLn $ case parse regex regstr of
    [] -> "Error compiling regex"
    ((reg, rem):xs) -> "/" ++ (take (length regstr - length rem) regstr) ++ "/" ++ " matches " ++ match reg text ++ "\n"

grepmain = do
  args <- getArgs
  regstr <- pure $ head args

  putStrLn $ case args of
    [] -> "Usage: hagrep PATTERN"
    (regstr:xs) -> case parse regex regstr of
      [] -> "Error compiling regex"
      ((reg, rem):ps) -> "compiled regex: /" ++ (take (length regstr - length rem) regstr) ++ "/"



dirmain = do
  reg <- pure $ fst $ head $ parse hackRegex "Cap"
  -- List everything in the directory (except . and ..) as pathnames
  dir <- listDirectory "."
  -- only keep the pathnames that are files (remove directories)
  files <- filterM doesFileExist dir
  -- only keep the files that don't have the excluded extensions
  toSearch <- pure $ filter isFileSearchable $ map pack files

  -- mapM :: (a -> m b) -> t a -> m (t b)
  -- mapM :: (String -> IO [String]) -> [String] -> IO [[String]]
  -- matchCollections :: [[s]]
  matchCollections <- mapM (fileSearch reg) (map unpack toSearch)
  mapM putStrLn $ concat matchCollections

dirmain2 = do
  reg <- pure $ fst $ head $ parse hackRegex "Cap"
  -- List everything in the directory (except . and ..) as pathnames
  dir <- listDirectory "."
  -- only keep the pathnames that are files (remove directories)
  files <- filterM doesFileExist dir
  -- only keep the files that don't have the excluded extensions
  toSearch <- pure $ filter isFileSearchable $ map pack files

  -- contents :: [[String]]; a list of files, each as a list of lines
  contents' <- mapM readFile $ map unpack toSearch
  contents <- pure $ map lines contents'
  -- contents' <- readFile $ unpack $ head toSearch
  -- contents <- pure $ [lines contents']
  printAllMatchesPure (filter $ hasMatch reg) contents


printAllMatchesPure :: ([String] -> [String]) -> [[String]] -> IO ()
printAllMatchesPure search [] = return ()
printAllMatchesPure search (x:xs) = do 
                                  matches <- pure $ search x
                                  mapM putStrLn matches
                                  printAllMatchesPure search xs

printAllMatches :: (String -> IO [String]) -> [String] -> IO ()
printAllMatches search [] = return ()
printAllMatches search (x:xs) = do 
                                  matches <- search x
                                  if length matches >=1 then print matches else pure ()
                                  printAllMatches search xs

-- filematchmaintest = do
--   reg <- pure $ fst $ head $ parse hackRegex "caneer"
--   matchLines <- fileSearch reg "treasure_island.html"
--   mapM putStrLn matchLines




isFileSearchable filename = (length split > 1) && (not $ elem ext ignoredExtensions)
  where
    split = splitOn (pack ".") filename
    ext = split !! 1
    ignoredExtensions = map pack ["exe", "o", "hi"]


-- given a regex and a filename,
--  list all lines that contain a match 
fileSearch :: Regex -> String -> IO [String]
fileSearch reg filename = fmap filterMatches $ readFile filename
  where
    filterMatches = (++ ["Finished searching " ++ show filename]) . filter (hasMatch reg) . lines


hasMatch :: Regex -> String -> Bool
hasMatch reg str = if rm == sm then rm else trace ("MATCH MISSED: " ++ str) rm
  where
    rm = not $ null $ parse reg str
    sm = isInfixOf "Cap" str

match :: Regex -> String -> String
match reg str = case parse reg str of
    [] -> "nothing"
    xs ->  fst $ head xs
-- match reg str = show $ map fst $ parse reg str

matches :: Regex -> String -> String
matches reg str = show $ map fst $ parse reg str







-- a hacky way to allow matches anywhere in a string
--    (rather than strictly at the start)
hackRegex :: Parser Regex
hackRegex = fmap hackPrepend regex

-- just add .*? before the regex
--  (and ignore whatever it matches)
hackPrepend :: Regex -> Regex
hackPrepend reg = do
  zeroormorenongreedy $ fmap singleton $ sat anychar
  reg

-- an element then more stuff OR a single element
regex :: Parser Regex
regex = (regelement <> regex) <|> pure emptyMatch

-- regchar at the end so that '(', '\', and '[' aren't interpreted as single characters
regelement :: Parser Regex
regelement = do
  x <- reggroup <|> regclass <|> regchar
  mod <- regmodifier
  return $ mod x

regchar :: Parser Regex
regchar = do
  x <- sat (not . isSpecial)
  return $ string [x]

regclass :: Parser Regex
-- take a predicate (Char -> Bool), 
--    select one character that satisfies it, 
--    treat it as a string of length 1
regclass = fmap (fmap singleton . sat) pred
  where 
    pred = do 
        char '[' 
        -- alternative value doesn't need to be +, can be anything
        --  #TODO something better here, like a Maybe Char or something
        comp <- char '^' <|> pure '+'
        x <- some regclassoption
        char ']'
        let p = anyp x
        return $ if comp == '^' then not . p else p
      <|> regspecialcharset

reggroup :: Parser Regex
reggroup = do
  char '('
  g <- regex
  char ')'
  return g

regclassoption :: Parser (Char -> Bool)
regclassoption = do -- 
    x <- sat (/= ']')
    char '-'
    y <- sat (/= ']')
    return $ (\ch -> (ord x <= ord ch) && (ord ch <= ord y))
  <|> regspecialcharset
  <|> do -- single normal character
    x <- sat $ not . (flip elem "]") 
    return $ (== x)

-- ==================================
--     CHAR SETS (and parser)
-- ==================================
regspecialcharset :: Parser (Char -> Bool)
regspecialcharset = do
    char '.'
    return anychar
  <|> do
    string "\\s"
    return whitespacechar
  <|> do
    string "\\S"
    return $ not .whitespacechar
  <|> do
    string "\\w"
    return wordchar
  <|> do
    string "\\W"
    return $ not . wordchar

whitespacechar = flip elem " \t\n\r\f"
anychar = charInRange ' ' '~'
wordchar = (charInRange '0' '9') ||| (charInRange 'a' 'z') ||| (charInRange 'A' 'Z') ||| (=='_')

charInRange x y = \ch -> ord x <= ord ch && ord ch <= ord y

-- ================================
--    Modifiers (one or more etc)
--      as functions (Regex -> Regex)
-- ================================

regmodifier:: Parser (Regex -> Regex)
regmodifier = do
      string "??"
      return zerooronenongreedy
    <|> do
      char '?'
      return zeroorone
    <|> do
      string "*?"
      return zeroormorenongreedy
    <|> do
      char '*'
      return zeroormore
    <|> do
      string "+?"
      return oneormorenongreedy
    <|> do
      char '+'
      return oneormore
    <|> pure id

zeroorone p = p <|> emptyMatch

zerooronenongreedy p = emptyMatch <|> p

oneormore p = P (\inp ->  concat $ map greedyOptions $ parse (some p) inp)

oneormorenongreedy p = P (\inp ->  (concat $ map nongreedyOptions $  parse (some p) inp))

zeroormore p = oneormore p <|> emptyMatch

zeroormorenongreedy p = emptyMatch <|> oneormorenongreedy p



-- should this be defined as "reverse nongreedyoptions" ?
greedyOptions :: ([String], String) -> [(String, String)]
greedyOptions ([], rem) = []
greedyOptions (xs, rem) = (mconcat xs, rem) : greedyOptions (init xs, last xs ++ rem)

nongreedyOptions :: ([String], String) -> [(String, String)]
nongreedyOptions ([], rem) = []
nongreedyOptions ((x:xs), rem) = (x, mconcat xs ++ rem) : (map (\(a,b) -> (x++a, b)) suffixes)
  where
    suffixes =  nongreedyOptions (xs, rem)


isSpecial = flip elem "+*?)"

emptyMatch :: Regex
emptyMatch = pure ""

safeHead [] = Nothing
safeHead (x:xs) = Just x

singleton :: Char -> String
singleton x = [x]

anyp :: [a -> Bool] -> (a -> Bool)
anyp [] val = False
anyp (x:xs) val = (x val) || anyp xs val

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p ||| q = \x -> p x || q x

nonep :: [a -> Bool] -> (a -> Bool)
nonep [] val = True
nonep (x:xs) val = (not $ x val) && nonep xs val
