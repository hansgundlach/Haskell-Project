
import Control.Monad (replicateM)



getLines :: Int -> IO [String]
getLines n = do
    replicateM n getLine

readNumbers :: String -> [Int]
readNumbers = map read . words

reading :: [String] -> [Int]
reading [] = []
reading (x:xs) = readNumbers x ++ reading xs


--print number of common divisors in coorect arrangment
printA :: [Int] -> IO()
printA x = do
       if null x
           then do  return()
          else do
            print $ finddivs $ gcdin (x!!0) (x!!1)
            printA $ tail $ tail x

-- mimimization step
help :: Integral a => a -> a
help n  =  fromIntegral $ floor $ sqrt (fromIntegral n)


is_square n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n::Double)


finddivs :: Int -> Int
finddivs n
      | (n ==1) = 1
      | (is_square n) = (length $ filter (\x -> (n `mod` x == 0)) [1..help n])*2 - 1
      |  otherwise = (length $ filter (\x -> (n `mod` x == 0)) [1..help n])*2

-- plain gcd calculation
gcdin :: Int -> Int -> Int
gcdin x y
    | ((x <1 ) || (y <1))  = 0
    | (x == y) = x
    | (x < y ) = gcdin x (y-x)
    | (x > y ) = gcdin (x-y) y

--read user input
main = do
   d <- getLine
   j <- getLines ( read d :: Int)
   printA $ reading j
