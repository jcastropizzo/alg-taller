doble x = 2 * x

suma x y = x + y

normaVectorial x y = sqrt(x^2+y^2)

funcionConstante x = 8

-- pattern matching (http://learnyouahaskell.com/syntax-in-functions)

absoluto :: Int -> Int
absoluto n | n < 0 = n * (-1)
           | otherwise = n

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y | absoluto(x) > absoluto(y) = absoluto(x)
                   | absoluto(y) > absoluto(x) = absoluto(y)
                   | otherwise = absoluto(y)

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | otherwise = z
          
algunoEs0 :: Int -> Int -> Bool
algunoEs0 x y =  (x == 0)||(y == 0)

algunoEs0Pattern :: Int -> Int -> Bool
algunoEs0Pattern _ 0 = True
algunoEs0Pattern 0 _ = True
algunoEs0Pattern _ _ = False

ambosSon0 :: Int -> Int -> Bool
ambosSon0 x y = (x == 0)&&(y == 0)

ambosSon0Pattern :: Int -> Int -> Bool
ambosSon0Pattern 0 0 = True
ambosSon0Pattern _ _ = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe n div = n `mod` div == 0

digitoUnidades :: Int -> Int
digitoUnidades x = x `mod` 10

digitoDecenas :: Int -> Int
digitoDecenas x = (x `div` 10) `mod` 10