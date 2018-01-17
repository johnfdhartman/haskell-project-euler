-- nextMultiple :: (Integral a) => a -> a
-- nextMultiple a
--   | (next `mod` 3 == 0 || next `mod` 5 == 0) = next
--   | otherwise = nextMultiple (next)
--   where next = a + 1
--
-- getMultiples :: Int -> [Int] -> [Int]
-- getMultiples total current
--   | (length current) < total = getMultiples total (
--     current ++ [(nextMultiple (last current))]
--   )
--   | otherwise = current
--
-- sumMultiples :: Int -> Int
-- sumMultiples total = sum (getMultiples total [3])

--Answered the wrong question lmao, tried to get the sum of the first 1000 multiples

getMultiples :: Int -> Int -> Int -> Int
getMultiples total runningSum current
  | current < total && (isMultiple current) = (
    getMultiples total (runningSum + current) (current + 1)
    )
  | current < total && not (isMultiple current) = (
    getMultiples total runningSum (current + 1)
    )
  | otherwise = runningSum
  where isMultiple current = (current `mod` 3 == 0 || current `mod` 5 == 0)

sumMultiples :: Int -> Int
sumMultiples total = getMultiples total 0 1
