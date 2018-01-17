-- Too slow to be feasible

-- primeFactors :: Int -> Int
-- primeFactors n
--   | factors == [n] = n
--   | otherwise = maximum [ primeFactors f | f <- factors, f /= n]
--   where factors = [m | m <- [2..n], n `mod` m == 0]
--

-- Faster without iteration?

smallestFactorHigherThan :: Int -> Int -> Int
smallestFactorHigherThan n m
  | n `mod` m == 0 = m
  | otherwise = smallestFactorHigherThan n (m + 1)

smallestFactor :: Int -> Int
smallestFactor n = smallestFactorHigherThan n 2

largestPrimeFactor :: Int -> Int
largestPrimeFactor n = let smallest = smallestFactor n in
  if smallest == n
    then n
  else largestPrimeFactor(n `quot` smallest)
