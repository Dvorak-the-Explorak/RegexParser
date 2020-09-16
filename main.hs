import Parsing


main = interact $
  show . solve


type Regex = [RegElement]
data RegElement = RegChar Char RegModifier | RegClass String RegModifier | RegCoClass String RegModifier | RegGroup Regex RegModifier
  deriving Show
data RegModifier = One | ZeroOrOne | ZeroOrMore | ZeroOrMoreNongreedy | OneOrMore | OneOrMoreNongreedy
  deriving Show



regex = some regelement
regelement = regchar <|> regclass <|> regcoclass <|> reggroup
regchar = do
  x <- sat (not . isSpecial)
  mod <- regmodifier
  return $ RegChar x mod
-- doesn't allow '^' in the class at all (should be allowed other than in the first position)
regclass = do
  char '['
  x <- some regclassoption
  char ']'
  mod <- regmodifier
  return $ RegClass x mod
regcoclass = do
  char '['
  char '^'
  x <- some regclassoption
  char ']'
  mod <- regmodifier
  return $ RegCoClass x mod
reggroup = do
  char '('
  g <- regex
  char ')'
  mod <- regmodifier
  return $ RegGroup g mod
regmodifier = do
      char '?'
      return ZeroOrOne
    <|> do
      char '*'
      char '?'
      return ZeroOrMoreNongreedy
    <|> do
      char '*'
      return ZeroOrMore
    <|> do
      char '+'
      char '?'
      return OneOrMoreNongreedy
    <|> do
      char '+'
      return OneOrMore
    <|> do return One




isSpecial = flip elem "[.(+*?"
isSpecialOption = flip elem "^-]"
regclassoption = sat (not . isSpecialOption)




solve :: String -> [(Regex, String)]
solve regstr = parse regex regstr
-- solve ls = case matches of
--     Nothing -> ["Error parsing regex!"]
--     Just xs -> xs
--   where
--     matches = do
--       exp <- safeHead ls
--       reg <- compile exp
--       rem <- safeTail ls
--       text <- Just $ recombine rem
--       return $ search reg text

safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail [] = Nothing
safeTail (x:xs) = Just xs

recombine [] = ""
recombine [x] = x 
recombine (x:y:xs) = x ++ "\n" ++ recombine (y:xs)

compile :: String -> Maybe Regex
compile = const Nothing

search :: Regex -> String -> [String]
search r s = []

