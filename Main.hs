import           Data.List.Ordered

main :: IO()
main = do
  content <- readFile "data.txt"
  let fileLines = lines content
  let sorted = sort [breakAtSpace x | x <- fileLines]
  let decimals = map (parsehex.reverse) sorted
  print $ decimals!!5

distance :: [Integer] -> [Integer]
distance = undefined

breakAtSpace :: String -> String
breakAtSpace(c:cs)
  |c == ' '  = []
  |otherwise = c : breakAtSpace cs

parsehex :: String -> Integer
parsehex = go
  where
    go []     = 0
    go (x:xs) = hexchar x + 16 * parsehex xs

hexchar :: Char -> Integer
hexchar '1' = 1
hexchar '2' = 2
hexchar '3' = 3
hexchar '4' = 4
hexchar '5' = 5
hexchar '6' = 6
hexchar '7' = 7
hexchar '8' = 8
hexchar '9' = 9
hexchar 'a' = 10
hexchar 'b' = 11
hexchar 'c' = 12
hexchar 'd' = 13
hexchar 'e' = 14
hexchar 'f' = 15
hexchar  _  = 0
