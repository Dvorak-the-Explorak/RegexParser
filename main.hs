import Parsing
import Data.Char (chr, ord)

main = interact $
  show . solve

type Regex = [RegElement]
data RegElement = RegChar Char RegModifier | RegClass String RegModifier | RegCoClass String RegModifier | RegGroup Regex RegModifier
  deriving Show
data RegModifier = One | ZeroOrOne | ZeroOrMore | ZeroOrMoreNongreedy | OneOrMore | OneOrMoreNongreedy
  deriving Show





regex = some regelement
-- coclass must come before class, that's how it handles the starting ^
-- regchar at the end so that '(', '\', and '[' aren't interpreted as single characters
regelement = regcoclass <|> regclass <|> reggroup <|> regchar
regclass = do
    char '['
    x <- some regclassoption
    char ']'
    mod <- regmodifier
    return $ RegClass (foldr (++) "" x) mod
  <|> do
    char '.'
    mod <- regmodifier
    return $ RegClass wildcard mod
  <|> do
    string "\\s"
    mod <- regmodifier
    return $ RegClass " \t\n" mod
  <|> do
    string "\\S"
    mod <- regmodifier
    return $ RegCoClass " \t\n" mod
regcoclass = do
  string "[^"
  x <- some regclassoption
  char ']'
  mod <- regmodifier
  return $ RegCoClass (foldr (++) "" x) mod
reggroup = do
  char '('
  g <- regex
  char ')'
  mod <- regmodifier
  return $ RegGroup g mod
regchar = do
  x <- sat (not . isSpecial)
  mod <- regmodifier
  return $ RegChar x mod
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

isSpecial = flip elem "+*?)"
wildcard = charRange 20 126
regclassoption = do
    x <- sat (/= ']')
    char '-'
    y <- sat (/= ']')
    return $ explode x y
  <|> do
    x <- sat $ not . (flip elem "]") 
    return [x]

solve :: String -> [(Regex, String)]
solve regstr = match reg "Hello, world!" parse regex regstr

explode :: Char -> Char -> String
explode x y = charRange (ord x) (ord y)

charRange :: Int -> Int -> String
charRange i j | i > j = ""
              | i == j = [chr i]
              | otherwise = chr i : (charRange (i+1) j)

safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail [] = Nothing
safeTail (x:xs) = Just xs

search :: Regex -> String -> [String]
search r s = []

