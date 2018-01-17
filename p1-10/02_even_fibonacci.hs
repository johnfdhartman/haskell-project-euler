sumEvenFibs :: Int -> (Int, Int) -> Int -> Int
sumEvenFibs highest (prev, current) runningSum
  | next >= highest = runningSum
  | even next = sumEvenFibs highest (current, next) runningSum + next
  | otherwise = sumEvenFibs highest (current, next) runningSum
  where next = prev + current

startSum :: Int -> Int
startSum highest = sumEvenFibs highest (1, 1) 0
