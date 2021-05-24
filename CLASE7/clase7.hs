type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece n (x:xs)
    | n == x = True
    | otherwise = pertenece n xs

agregar ::Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise = x:c

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True
incluido _ [] = False
incluido (x:xs) conjuntoIncluyente
    | pertenece x conjuntoIncluyente = incluido xs conjuntoIncluyente
    | otherwise = False

iguales :: Set Int -> Set Int -> Bool
iguales a b = incluido a b && incluido b a

union :: Set Int -> Set Int -> Set Int
union [] a = a
union a [] = a
union (a:as) bs
    | pertenece a bs = union as bs
    | otherwise = (a: union as bs)

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] _ = []
interseccion _ [] = []
interseccion (a:as) bs
    | pertenece a bs = (a: interseccion as bs)
    | otherwise = interseccion as bs

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] a = []
diferencia a [] = a
diferencia (a:as) bs
    | pertenece a bs = diferencia as bs
    | otherwise = (a: diferencia as bs)

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica a b = diferencia (union a b) (interseccion a b)

existeEnC :: Set Int -> Set (Set Int) -> Bool
existeEnC _ [] = False
existeEnC x (y:ys)
    | iguales x y = True
    | otherwise = existeEnC x ys

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] y = y
unionC x [] = x
unionC (x:xs) y
    | existeEnC x y = unionC xs y
    | otherwise = (x:unionC xs y)

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos _ [] = []
agregarATodos n (x:xs) = unionC [(agregar n x)] (agregarATodos n xs)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

partesN :: Int -> Set (Set Int)
partesN n = partes [1..n]

generarTuplaConN :: Int -> Set Int -> Set (Int,Int)
generarTuplaConN _ [] = []
generarTuplaConN n (x:xs) = ((n,x):generarTuplaConN n xs)

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] _ = []
productoCartesiano (x:xs) y = (generarTuplaConN x y) ++ (productoCartesiano xs y)