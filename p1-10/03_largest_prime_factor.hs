primeFactors :: Int -> Int
primeFactors n
  | factors == [n] = n
  | otherwise = maximum [ primeFactors f | f <- factors, f /= n]
  where factors = [m | m <- [2..n], n `mod` m == 0]
