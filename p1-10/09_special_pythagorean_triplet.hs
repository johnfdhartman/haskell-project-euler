import Data.List

bFromA :: (Integral a) => a -> a
bFromA a = ( 1000 * (a - 500) ) `quot` (a - 1000)

cFromA :: (Integral x, RealFloat y) => x -> y
cFromA a = sqrt( fromIntegral (a^2 + b^2) )
  where b = bFromA a

isInt :: (RealFloat x) => x -> Bool
isInt x = x == fromIntegral (round x)

specialTripletA :: Integral x => Maybe x
specialTripletA = find ( \a -> isInt (cFromA a) ) [1..1000]
