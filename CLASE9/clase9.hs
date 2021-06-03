type Ecuacion = (Int,Int,Int)

type Solucion = (Int, Int)

esPrimo :: Int -> Bool
esPrimo n
    | n < 2 = False
    | otherwise = ((menorDivisor n) == n)

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde (2, n)

menorDivisorDesde :: (Int, Int) -> Int
menorDivisorDesde (k, n)
    | mod n k == 0 = k
    | otherwise = menorDivisorDesde ((k + 1), n)

----------------------------------------------------------------

esDivisiblePor :: Int -> Int -> Bool
esDivisiblePor n d = (n `mod` d) == 0

mcd :: Int -> Int -> Int
mcd n 0 = n
mcd 0 n = n
mcd n1 n2
    | n1 < 0 || n2 < 0 = mcd (abs n1) (abs n2)
    | otherwise = mcd n2 (n1 `mod` n2)

tieneSolucion :: Ecuacion -> Bool
tieneSolucion (a, b, m)
    | b `mod` d == 0 = True
    | otherwise = undefined
    where d = mcd a m

reducirEcuacion :: Ecuacion -> Ecuacion
reducirEcuacion (a, b, m) = (a `div` d, b `div` d, m `div` d) 
    where 
        d = mcd a m

solucionEc :: Ecuacion -> Solucion
solucionEc (a, b, m)
    | not (tieneSolucion (a, b, m)) = undefined
    | otherwise = ( (buscarValorDivisible b' m' a') `div` a', m')
    where 
        (a', b', m') = reducirEcuacion (a, b, m)

buscarValorDivisible :: Int -> Int -> Int -> Int
buscarValorDivisible base m divisor 
    | base `mod` divisor == 0 = base 
    | otherwise = buscarValorDivisible (base + m) m divisor

sistemaSimplifEquiv :: [Ecuacion] -> [Solucion]
sistemaSimplifEquiv [] = []
sistemaSimplifEquiv (x:xs) = (solucionEc x) : sistemaSimplifEquiv xs

todosLosPrimosMalos :: [Solucion] -> [Int]
todosLosPrimosMalos soluciones = todosLosPrimosQueDividenAMasDeUnM soluciones

existe :: [Int] -> Int -> Bool
existe [] _ = False
existe (x:xs) n
    | n == x = True
    | otherwise = existe xs n

todosLosPrimosQueDividenAMasDeUnM :: [Solucion] -> [Int]
todosLosPrimosQueDividenAMasDeUnM [] = []
todosLosPrimosQueDividenAMasDeUnM soluciones = filtrarNoRepetidos (obtenerDivisoresPrimos(obtenerTodosLosM soluciones ))

obtenerTodosLosM :: [Solucion] -> [Int]
obtenerTodosLosM [] = []
obtenerTodosLosM ((r, m):xs) = m: obtenerTodosLosM xs

obtenerDivisoresPrimos :: [Int] -> [Int]
obtenerDivisoresPrimos [] = []
obtenerDivisoresPrimos (x:xs) = filtrarNoPrimos (obtenerDivisoresDesde x (x)) ++ obtenerDivisoresPrimos xs

obtenerDivisoresDesde :: Int -> Int -> [Int]
obtenerDivisoresDesde _ 1 = []
obtenerDivisoresDesde n div
    | n `mod` div == 0 = div : obtenerDivisoresDesde n (div - 1)
    | otherwise = obtenerDivisoresDesde n (div - 1)

filtrarNoPrimos :: [Int] -> [Int]
filtrarNoPrimos [] = []
filtrarNoPrimos (x:xs) 
    | esPrimo x = x : filtrarNoPrimos xs
    | otherwise = filtrarNoPrimos xs

filtrarNoRepetidos :: [Int] -> [Int]
filtrarNoRepetidos [] = []
filtrarNoRepetidos (x:xs)
    | existe xs x = x : filtrarNoRepetidos(quitarNDeLaLista x xs)
    | otherwise =  filtrarNoRepetidos(xs)

quitarNDeLaLista :: Int -> [Int] -> [Int]
quitarNDeLaLista _ [] = []
quitarNDeLaLista n (x:xs)
    | n == x = quitarNDeLaLista n xs
    | otherwise = x : quitarNDeLaLista n xs

    solucSistemaPotenciasPrimo :(