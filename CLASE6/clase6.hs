sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria l = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud [] = 0
longitud l = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece num (x : xs) 
    | x == num = True
    | otherwise = pertenece num xs

agregarAlFinal :: Int -> [Int] -> [Int]
agregarAlFinal x [] = [x]
agregarAlFinal n (x:xs) = (x : agregarAlFinal n xs)

ultimoValor :: [Int] -> Int
ultimoValor [x] = x
ultimoValor (x:xs) = ultimoValor xs

removerUltimoValor :: [Int] -> [Int]
removerUltimoValor [x] = []
removerUltimoValor (x:xs) = (x :removerUltimoValor xs)

reverso :: [Int] -> [Int]
reverso [] = []
reverso lista = (ultimoValor lista: reverso (removerUltimoValor lista))

listaDe1AMenos100 :: [Int]
listaDe1AMenos100 = reverso [-100..1]

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

sumarN :: Int -> [Int] -> [Int]
sumarN _ [] = []
sumarN n (x:xs) = (x+n: sumarN n xs)

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []
sumarElPrimero list = sumarN (head list) list

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo [] = []
sumarElUltimo list = sumarN (ultimoValor list) list

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs)
    | x `mod` 2 == 0 = (x : pares xs)
    | otherwise = pares xs

quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar n (x:xs)
    | n == x = xs
    | otherwise = (x: quitar n xs)

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n (x:xs)
    | n == x = quitarTodas n xs
    | otherwise = (x: quitarTodas n xs)

cantidadDeRepeticiones :: Int -> [Int] -> Int
cantidadDeRepeticiones _ [] = 0
cantidadDeRepeticiones n (x:xs)
    | n == x = 1 + cantidadDeRepeticiones n xs
    | otherwise = cantidadDeRepeticiones n xs

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (x: xs) 
    | cantidadDeRepeticiones x xs == 0 = hayRepetidos xs
    | otherwise = True

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x: xs) = (x: eliminarRepetidosAlFinal (quitarTodas x xs))

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = []
eliminarRepetidosAlInicio (x:xs)
    | cantidadDeRepeticiones x xs == 0 = (x: eliminarRepetidosAlInicio xs)
    | otherwise = eliminarRepetidosAlInicio xs

maximo :: [Int] -> Int
maximo [] = 0
maximo [x] = x
maximo (x:y:xs) 
    | x > y = maximo (x:xs)
    | otherwise = maximo (y:xs)

minimo :: [Int] -> Int
minimo [] = 0
minimo [x] = x
minimo (x:y:xs) 
    | x > y = minimo (y:xs)
    | otherwise = minimo (x:xs)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar lista = (minimo lista : ordenar (quitar (minimo lista) lista))

concatenarPrelude :: [Int] -> [Int] -> [Int]
concatenarPrelude l1 l2 = l1 ++ l2

concatenar :: [Int] -> [Int] -> [Int]
concatenar [] [] = []
concatenar (x:xs) l2 =(x: concatenar xs l2)
concatenar [] (x:xs) = (x: concatenar [] xs)

zipi :: [a] -> [b] -> [(a,b)]
zipi [] _ = []
zipi _ [] = []
zipi (a:as) (b:bs) = ((a,b):zipi as bs)