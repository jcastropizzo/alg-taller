esPar :: Integer -> Bool
esPar n = ( n `mod` 2) == 0

esPrimo :: Integer -> Bool
esPrimo n
    | n < 2 = False
    | otherwise = ((menorDivisor n) == n)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde (2, n)

menorDivisorDesde :: (Integer, Integer) -> Integer
menorDivisorDesde (k, n)
    | mod n k == 0 = k
    | otherwise = menorDivisorDesde ((k + 1), n)

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos num
    | num <= 2 = False
    | otherwise = esSumaDeDosPrimosIterador (2, num)
    where
        esSumaDeDosPrimosIterador :: (Integer, Integer) -> Bool
        esSumaDeDosPrimosIterador (x, objetivo)
            | ((2*x) > objetivo) = False -- si ya validó todos los x,y con 2 <= x < objetivo/2, se probaron todas las combinaciones hasta la x,x
            | (esPrimo x) && (esPrimo (objetivo-x)) = True
            | otherwise = esSumaDeDosPrimosIterador ((x+1), objetivo)

-- EJ 1
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n
    | (esPar n) && (n > 2) && (esSumaDeDosPrimos n) = True
    | otherwise = False

-- EJ 2
-- Precondiciones:
--    Entrada: n > 2
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n
    | (n `mod` 2) /= 0 = verificarConjeturaHasta (n-1)
    | satisfaceGoldbach n = verificarConjeturaHasta (n-2)
    | n == 2 = True
    | otherwise = False

-- EJ 3
-- Precondiciones:
--    Entrada: n > 2
descomposicionEnPrimos :: Integer -> (Integer,Integer)
descomposicionEnPrimos n = descomposicionEnPrimosIterador (2,n)
    where 
        descomposicionEnPrimosIterador :: (Integer, Integer) -> (Integer, Integer)
        descomposicionEnPrimosIterador (x, objetivo)
            | x > objetivo = (-1,-1) -- guarda para evitar loop infinito si no tiene descomposición en primos
            | (esPrimo x) && (esPrimo (objetivo-x)) = (x, objetivo-x)
            | otherwise = descomposicionEnPrimosIterador(x+1, objetivo)

-- EJ 4
-- Precondiciones:
--    Entrada: n > 2
numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n = descomposicionEnPrimosIterador (2, n)
    where 
        descomposicionEnPrimosIterador :: (Integer, Integer) -> Integer
        descomposicionEnPrimosIterador (x, objetivo)
            | (objetivo - x) < 2 = 0 -- no tiene sentido seguir el loop, siendo 2 el primo más pequeño
            | esPrimo x && esPrimo (objetivo - x) = 1 + descomposicionEnPrimosIterador (x + 1, objetivo)
            | otherwise = descomposicionEnPrimosIterador (x + 1, objetivo)