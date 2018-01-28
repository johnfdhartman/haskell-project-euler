sumSquares :: Int -> Int
sumSquares n = sum [ m^2 | m <- [1..n] ]

squareSum :: Int -> Int
squareSum n = (sum [1..n])^2

sumSquareDifference :: Int -> Int
sumSquareDifference n = (squareSum n) - (sumSquares n)
