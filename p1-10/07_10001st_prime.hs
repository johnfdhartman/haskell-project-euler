-- slow as fuck

import Data.List
import Data.Maybe

isPrime :: Int -> Bool
isPrime n = not (0 `elem` [n `mod` m | m <- [2..(n-1)]])

nextPrime :: [Int] -> [Int]
nextPrime ps = ps ++ [fromMaybe 0 newP]
  where newP = find isPrime [(p + 1).. ]
        p = last ps

firstNPrimes :: Int -> [Int] -> [Int]
firstNPrimes n ps
  | length ps >= n = ps
  | otherwise = firstNPrimes n (nextPrime ps)

nthPrime :: Int -> Int
nthPrime n = last (firstNPrimes n [2])
