import Parsing
import Data.Char (chr, ord)

main = interact $
  solve . lines

-- This version is an attempt at turning the regex string into a parser
-- ie, we attempt to write a Parser (Parser String)

type Regex = Parser String

-- the regex parser type parses a string out of a string
-- regex :: Parser Regex
regex :: Parser Regex
regex = regelement


-- -- regchar at the end so that '(', '\', and '[' aren't interpreted as single characters
-- regelement = regcoclass <|> regclass <|> reggroup <|> regchar

regchar :: Parser Regex
regchar = do
  x <- sat (not . isSpecial)
  return $ string [x]

regelement :: Parser Regex
regelement = do
  x <- reggroup <|> regclass <|> regchar
  -- x <- regclass <|> regchar
  mod <- regmodifier
  return $ mod x

regclass :: Parser Regex
regclass = do
    char '['
    comp <- char '^' <|> pure '+'
    x <- some regclassoption
    char ']'
    return (case comp of
        '+' -> fmap singleton $ sat $ anyp x
        '^' -> fmap singleton $ sat $ not . anyp x)
  <|> do
    char '.'
    return $ fmap singleton $ sat (\ch -> 20 <= ord ch && ord ch <= 126)
  <|> do
    string "\\s"
    return $ fmap singleton $ sat (flip elem " \t\n\r\f") 
  <|> do
    string "\\S"
    return $ fmap singleton $ sat $ not . (flip elem " \t\n\r\f") 

reggroup :: Parser Regex
reggroup = do
  char '('
  g <- regex
  char ')'
  return g

regclassoption :: Parser (Char -> Bool)
regclassoption = do
    x <- sat (/= ']')
    char '-'
    y <- sat (/= ']')
    return $ (\ch -> ord x <= ord ch && ord ch <= ord y)
  <|> do
    x <- sat $ not . (flip elem "]") 
    return $ (== x)

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
    <|> do return id

zeroorone :: Regex -> Regex
-- :: (Parser String) -> (Parser String)
zeroorone p = P (\inp -> case (parse (some p) inp) of
                  [] -> [("", inp)]
                  (x:xs) -> [(head $ fst x, snd x)] ++ [("", inp)])

zerooronenongreedy :: Regex -> Regex
-- :: (Parser String) -> (Parser String)
zerooronenongreedy p = P (\inp -> case (parse (some p) inp) of
                  [] -> [("", inp)]
                  (x:xs) -> [("", inp)] ++ [(head $ fst x, snd x)])

oneormore :: Regex -> Regex
-- :: (Parser String) -> (Parser String)
oneormore p = P (\inp -> case (parse (some p) inp) of
                  [] -> []
                  (x:xs) -> greedyOptions x)

oneormorenongreedy :: Regex -> Regex
oneormorenongreedy p = P (\inp -> case (parse (some p) inp) of
                  [] -> []
                  (x:xs) -> nongreedyOptions x)

zeroormore :: Regex -> Regex
-- :: (Parser String) -> (Parser String)
zeroormore p = P (\inp -> case (parse (some p) inp) of
                  [] -> [("", inp)]
                  (x:xs) -> greedyOptions x ++ [("", inp)])

zeroormorenongreedy :: Regex -> Regex
zeroormorenongreedy p = P (\inp -> case (parse (some p) inp) of
                  [] -> [("", inp)]
                  (x:xs) -> [("", inp)] ++ nongreedyOptions x)

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

solve :: [String] -> String
solve [] = "No input"
solve [x] = "No search text given"
solve (regstr:text:xs) = case safeHead parsers of
  Nothing -> "Error compiling regex"
  Just (reg,rem) -> "/" ++ (take (length regstr - length rem) regstr) ++ "/" ++ " matches \"" ++ match reg text ++ "\"\n"
  where
    parsers = parse regex regstr

match :: Regex -> String -> String
match reg str = case result of
    Nothing -> "nothing"
    Just x -> x
  where result = do
                  (result,rem) <- safeHead $ parse reg str
                  return result
-- match reg str = show $ parse reg str


safeHead [] = Nothing
safeHead (x:xs) = Just x

singleton :: Char -> String
singleton x = [x]

anyp :: [a -> Bool] -> (a -> Bool)
anyp [] val = False
anyp (x:xs) val = (x val) || anyp xs val

allp :: [a -> Bool] -> (a -> Bool)
allp [] val = False
allp (x:xs) val = (x val) && anyp xs val
