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
-- regex = do 
--   elems <- some regchar
--   return $ mconcat elems
regex :: Parser Regex
regex = fmap zeroormore regchar


-- -- coclass must come before class, that's how it handles the starting ^
-- -- regchar at the end so that '(', '\', and '[' aren't interpreted as single characters
-- regelement = regcoclass <|> regclass <|> reggroup <|> regchar



regchar :: Parser Regex
regchar = do
  x <- sat (not . isSpecial)
  mod <- regmodifier
  return (case mod of 
    One -> string [x]
    ZeroOrOne -> string [x] <|> do return ""
    ZeroOrMore -> many $ char x
    OneOrMore -> some $ char x
    _ -> undefined)


regmodifier = do
      char '?'
      return ZeroOrOne
    <|> do
      string "*?"
      return ZeroOrMoreNongreedy
    <|> do
      char '*'
      return ZeroOrMore
    <|> do
      string "+?"
      return OneOrMoreNongreedy
    <|> do
      char '+'
      return OneOrMore
    <|> do return One


oneormore :: Regex -> Regex
-- :: (Parser String) -> (Parser String)
oneormore p = P (\inp -> case (parse (some p) inp) of
                  [] -> []
                  (x:xs) -> greedyOptions x)

zeroormore :: Regex -> Regex
-- :: (Parser String) -> (Parser String)
zeroormore p = P (\inp -> case (parse (some p) inp) of
                  [] -> []
                  (x:xs) -> greedyOptions x ++ [("", inp)])


greedyOptions :: ([String], String) -> [(String, String)]
greedyOptions ([], rem) = []
greedyOptions (xs, rem) = (mconcat xs, rem) : greedyOptions (init xs, last xs ++ rem)

-- nongreedyOptions :: ([String], String) -> [(String, String)]
-- nongreedyOptions ([], rem) = []
-- nongreedyOptions ((x:xs), rem) = (x, mconcat xs ++ rem) : map (x ++) suffixes
  -- where
  --   suffixes =  nongreedyOptions (xs, rem)



-- regex = some regelement
-- -- coclass must come before class, that's how it handles the starting ^
-- -- regchar at the end so that '(', '\', and '[' aren't interpreted as single characters
-- regelement = regcoclass <|> regclass <|> reggroup <|> regchar
-- regclass = do
--     char '['
--     x <- some regclassoption
--     char ']'
--     mod <- regmodifier
--     return $ RegClass (foldr (++) "" x) mod
--   <|> do
--     char '.'
--     mod <- regmodifier
--     return $ RegClass wildcard mod
--   <|> do
--     string "\\s"
--     mod <- regmodifier
--     return $ RegClass " \t\n" mod
--   <|> do
--     string "\\S"
--     mod <- regmodifier
--     return $ RegCoClass " \t\n" mod
-- regcoclass = do
--   string "[^"
--   x <- some regclassoption
--   char ']'
--   mod <- regmodifier
--   return $ RegCoClass (foldr (++) "" x) mod
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

isSpecial = flip elem "+*?)"
-- wildcard = charRange 20 126
-- regclassoption = do
--     x <- sat (/= ']')
--     char '-'
--     y <- sat (/= ']')
--     return $ explode x y
--   <|> do
--     x <- sat $ not . (flip elem "]") 
--     return [x]

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


safeHead [] = Nothing
safeHead (x:xs) = Just x