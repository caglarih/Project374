import           Data.List.Ordered
import           System.Environment

main :: IO()
main = do
  arg <- getArgs
  content <- readFile ("data/zzz_" ++ head arg ++ "result.txt")
  let fileLines = lines content
  let sorted = sort [breakAtSpace x | x <- fileLines]
  let decimals = map (parsehex.reverse) sorted
  print $ div ((sum.distance) decimals) (foldr (\a -> (+) 1) 0 decimals)
  print $ distribute (map (\t -> div t (16^((length.head)sorted-2))) decimals) [0]

distance :: [Integer] -> [Integer]
distance []         = []
distance (x1:x2:xs) = (x2-x1) : distance (x2:xs)
distance _          = []

distribute :: [Integer] -> [Integer] -> [Integer]
distribute [] acc  = acc
distribute xs'@(x:xs) acc@(a:c)
  | x > 2        = distribute (map (\t -> t - 2) xs') (0:acc)
  | x > 0        = distribute (map (\t -> t - 2) xs) (1:acc)
  | otherwise    = distribute xs ((a+1):c)

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
