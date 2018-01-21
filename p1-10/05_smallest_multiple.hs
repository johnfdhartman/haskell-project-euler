-- algorithm: multiply numbers 1 through 20, but divide the *new* numbers
-- by smaller numbers with cofactors
-- i.e smallestMultiple 10 =  2 * 3 * (4 / 2) * 5 * (6/3)/2 * 7 * ...

-- smallestMultiple :: Int -> Int
-- smallestMultiple n = foldl (*) (map divideBySmallerFactors [1..n]) 1


-- wrong:
-- divideBySmallerFactors :: Int -> Int
-- divideBySmallerFactors n = foldl checkFactor n [1..(n-1)]
--

smallestMultiple :: Int -> Int
smallestMultiple n = foldl (*) 1 (foldableFactors n [1])


foldableFactors :: Int -> [Int] -> [Int]
foldableFactors n facs
  | n == length facs = facs
  | otherwise = foldableFactors n (facs ++ [nextFoldableFactor facs])


nextFoldableFactor :: [Int] -> Int
nextFoldableFactor folded = foldl checkFactor (1 + length folded) folded

checkFactor :: Int -> Int -> Int
checkFactor n m
  | n `mod` m == 0 = n `quot` m
  | otherwise = n
