f1 x y z = x ** y + z <= x+y ** z

f2 x y = ( sqrt x) / ( sqrt y)

f3 x y = div ( sqrt x) ( sqrt y)

f4 x y z 
    | x == y = z
    | x ** y == y = z
    | otherwise = z

f5 x y z 
    | x == y = z
    | x ** y == y = x
    | otherwise = y
    
cinco :: Int
cinco = 5
-- f3 cinco cinco


estanRelacionados :: Int -> Int -> Bool
estanRelacionados x y
    | inFirstInterval x && inFirstInterval y = True
    | inSecondInterval x && inSecondInterval y = True
    | inThirdInterval x && inThirdInterval y = True
    | otherwise = False
    where 
        inFirstInterval w = w <= 3
        inSecondInterval w = w > 3 && w <= 7
        inThirdInterval w = w > 7

prodInt :: (Num a) => (a, a) -> (a, a) -> a
prodInt (vx, vy) (wx,wy) = vx * wx + vy * wy

todoMenor :: (Ord a) => (a, a) -> (a, a) -> Bool
todoMenor (vx, vy) (wx, wy) 
    | vx < wx && vy < wy = True
    | otherwise = False

distanciaPuntos :: (Floating a) => (a, a) -> (a, a) -> a
distanciaPuntos (vx, vy) (wx, wy) = sqrt (((vx-wx)^2)+((vy-wy)^2))

sumaTerna :: (Num a) => (a, a, a) -> a
sumaTerna (x, y, z) = x + y + z

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z)
    | esPar x = 1
    | esPar y = 2
    | esPar z = 3
    | otherwise = 4
    where
        esPar :: Int -> Bool
        esPar w = (w `rem` 2) == 0

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

invertir :: (a, b) -> (b, a)
invertir (x, y) = (y, x)