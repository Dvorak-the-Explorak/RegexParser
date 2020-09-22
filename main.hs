{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
import Parsing
import Data.Char (chr, ord)  
import System.Environment

-- main = interact $
--   solve . lines

main = testmain

testmain = do
  regstr <- getLine
  text <- getLine
  let parsers = parse regex regstr
  putStrLn $ case parse regex regstr of
    [] -> "Error compiling regex"
    ((reg, rem):xs) -> "/" ++ (take (length regstr - length rem) regstr) ++ "/" ++ " matches \"" ++ match reg text ++ "\"\n"

grepmain = do
  args <- getArgs
  putStrLn $ head args

-- This version is an attempt at turning the regex string into a parser
-- ie, we attempt to write a Parser (Parser String)

type Regex = Parser String





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
regclass = do
    char '['
    comp <- char '^' <|> pure '+'
    x <- some regclassoption
    char ']'
    let p = anyp x
    return $ fmap singleton $ sat (if comp == '^' then not . p else p)
  <|> do
    pred <- regspecialcharset
    return $ fmap singleton $ sat pred

reggroup :: Parser Regex
reggroup = do
  char '('
  g <- regex
  char ')'
  return g

-- regmetachar :: Parser (Char -> Bool)
-- regmetachar = do
  

regclassoption :: Parser (Char -> Bool)
regclassoption = do
    x <- sat (/= ']')
    char '-'
    y <- sat (/= ']')
    return $ (\ch -> (ord x <= ord ch) && (ord ch <= ord y))
  <|> regspecialcharset
  <|> do
    x <- sat $ not . (flip elem "]") 
    return $ (== x)

-- ==================================
--     CHAR SETS :: Char -> Bool
--      (and a parser)
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


-- ================================
--    MODIFIERS :: Regex -> Regex
-- ================================

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

regcharrange :: Char -> Char -> Regex
regcharrange x y = fmap singleton $ sat (\ch -> ord x <= ord ch && ord ch <= ord y)


match :: Regex -> String -> String
match reg str = case parse reg str of
    [] -> "nothing"
    xs ->  fst $ head xs

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
