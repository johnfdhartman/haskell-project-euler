isPalindrome :: Int -> Bool
isPalindrome int = (str == reverse str)
  where str = show int

highestTriplePalindromeLessThan :: Int -> Int
highestTriplePalindromeLessThan n
  | (isPalindrome (n - 1) && hasTripleFactors (n - 1)) = n - 1
  | otherwise = highestTriplePalindromeLessThan (n - 1)


hasTripleFactors :: Int -> Bool
hasTripleFactors n = fst (hasTripleFactorLessThan n 1000)

hasTripleFactorLessThan :: Int -> Int -> (Bool, Int)
hasTripleFactorLessThan n m
  | next < 100 = (False, 0)
  | (n `mod` next == 0 && isTriplePair next (n `quot` next)) = (True, next)
  | otherwise = hasTripleFactorLessThan n next
  where next = m - 1

isTriplePair :: Int -> Int -> Bool
isTriplePair a b = (a > 99 && a < 1000 && b > 99 && b < 1000)
