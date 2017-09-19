-- Done for hackerank ad hoc challanges common divisor

-- "First line of input contains an integer, T, which represent the number of test cases.
--Then follows T lines. Each line contains two space separated integers, M L, representing the points earned by Mario and Luigi
--, respectively" .

import Control.Monad (replicateM)

getLines :: Int -> IO [String]
getLines n = do
    replicateM n getLine

readNumbers :: String -> [Int]
readNumbers = map read . words

reading :: [String] -> [Int]
reading [] = []
reading (x:xs) = readNumbers x ++ reading xs


--print number of common divisors in correct arrangment
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

-- determine if a guven integer is square
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

--read user input and print result
main = do
   d <- getLine
   j <- getLines ( read d :: Int)
   printA $ reading j
