
import Data.List
import Data.List (intercalate)
import System.Random

-- factorial method for computation
factorial 0 = 1
factorial 1 = 1
factorial n =  n*factorial (n-1)


-- combination of k out of n objects
n `choose` k
    | (k == n)  = 1
    | k < 0     = 0
    | k > n     = 0
    | otherwise = factorial n `div` (factorial k * factorial (n-k))

--generate pascal triangle
pasc n  = [ n `choose` k |  k <- [0..n]]
trian n = [pasc n| n <- [0..n-1]]


printer :: [Int] -> IO[()]
printer x = do
  mapM putStrLn $ mapM show x

trying :: [Int] -> String
trying [] = ""
trying (x:xs) =
  show(x) ++ " " ++ trying(xs)


doesit :: [[Int]] -> IO ()
doesit x = do
  putStrLn $ id trying $ head x
  if (tail x) == []
      then do putStrLn ""
      else do doesit $ tail x

main = do
    x <- getLine
    let m = read x :: Int
    doesit $ trian m
