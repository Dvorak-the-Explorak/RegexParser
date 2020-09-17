import Parsing
import Data.Char (chr, ord)

main = interact $
  solve . lines

-- This version is an attempt at turning the regex string into a parser
-- ie, we attempt to write a Parser (Parser String)

type Regex = Parser String


data RegModifier = One | ZeroOrOne | ZeroOrMoreNongreedy | ZeroOrMore | OneOrMoreNongreedy | OneOrMore

-- the regex parser type parses a string out of a string
-- regex :: Parser Regex
regex :: Parser Regex
regex = regelement


-- -- coclass must come before class, that's how it handles the starting ^
-- -- regchar at the end so that '(', '\', and '[' aren't interpreted as single characters
-- regelement = regcoclass <|> regclass <|> reggroup <|> regchar

regchar :: Parser Regex
regchar = do
  x <- sat (not . isSpecial)
  return $ string [x]

regelement :: Parser Regex
regelement = do
  x <- regcoclass <|> regclass <|> regchar
  mod <- regmodifier
  return $ mod x


regclass = do
    char '['
    x <- some regclassoption
    -- x :: [Regex] (each a single character pattern)
    char ']'
    return $ fmap singleton $ sat $ anyp x
  -- <|> do
  --   char '.'
  --   return $ RegClass wildcard
  -- <|> do
  --   string "\\s"
  --   mod <- regmodifier
  --   return $ RegClass " \t\n" mod
  -- <|> do
  --   string "\\S"
  --   mod <- regmodifier
  --   return $ RegCoClass " \t\n" mod

regcoclass :: Parser (Regex)
regcoclass = do
    string "[^"
    x <- some regclassoption
    -- x :: [Regex] (each a single character pattern)
    char ']'
    return $ fmap singleton $ sat $ not . anyp x

-- regcoclass :: Parser (Regex)
-- regcoclass = do
--     string "[^"
--     x <- some regcoclassoption
--     -- x :: [Regex] (each a single character pattern)
--     char ']'
--     return $ foldr1 (<|>) x


-- regcoclassoption :: Parser (Regex)
-- regcoclassoption = do
--     x <- sat (/= ']')
--     char '-'
--     y <- sat (/= ']')
--     return $ fmap singleton $ sat $ not . (\ch -> ord x <= ord ch && ord ch <= ord y)
--   <|> do
--     x <- sat $ not . (flip elem "]") 
--     -- return $ string [x]
--     return $ fmap singleton $ sat (/= x)

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
-- wildcard = charRange 20 126



-- regex = some regelement
-- -- coclass must come before class, that's how it handles the starting ^
-- -- regchar at the end so that '(', '\', and '[' aren't interpreted as single characters
-- regelement = regcoclass <|> regclass <|> reggroup <|> regchar

-- reggroup = do
--   char '('
--   g <- regex
--   char ')'
--   mod <- regmodifier
--   return $ RegGroup g mod
-- regmodifier = do
--       char '?'
--       return ZeroOrOne
--     <|> do
--       string "*?"
--       return ZeroOrMoreNongreedy
--     <|> do
--       char '*'
--       return ZeroOrMore
--     <|> do
--       string "+?"
--       return OneOrMoreNongreedy
--     <|> do
--       char '+'
--       return OneOrMore
--     <|> do return One


solve :: [String] -> String
solve [] = "No input"
solve [x] = "No search text given"
solve (regstr:text:xs) = case safeHead parsers of
  Nothing -> "Error compiling regex"
  Just (reg,rem) -> "/" ++ (take (length regstr - length rem) regstr) ++ "/" ++ " matches \"" ++ match reg text ++ "\"\n"
  where
    parsers = parse regex regstr


-- solve regstr = case safeHead parsers of
--   Nothing -> "Error compiling regex"
--   Just (reg,rem) -> "/" ++ (take (length regstr - length rem) regstr) ++ "/" ++ " matches \"" ++ match reg  "Hello, world!" ++ "\"\n"
--   where
--     parsers = parse regex regstr

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
anyp [] val = True
anyp (x:xs) val = (x val) || anyp xs val

allp :: [a -> Bool] -> (a -> Bool)
allp [] val = False
allp (x:xs) val = (x val) && anyp xs val