first ::(a, b, c) -> a
first (a, b, c) = a
second ::(a, b, c) -> b
second (a, b, c) = b
third ::(a, b, c) -> c 
third (a, b, c) = c


esPar :: Integer -> Bool
esPar n = ( n `mod` 2) == 0

esPrimo :: Integer -> Bool
esPrimo num
    | num < 0 = False
    | num == 0 = True
    | otherwise = esPrimoIterador (num,num)
    where
        esPrimoIterador :: (Integer, Integer) -> Bool
        esPrimoIterador (baseNum, currentNum)
            | currentNum <= 1 = True -- Es primo, llegó al final del loop
            | baseNum == currentNum = esPrimoIterador (baseNum, currentNum-1)
            | ((baseNum `mod` currentNum) == 0)
                && (baseNum /= currentNum) = False
            | otherwise = esPrimoIterador (baseNum, currentNum-1)

esSumaDeDosPrimos :: Integer -> Bool
esSumaDeDosPrimos num
    | num <= 2 = True
    | otherwise = esSumaDeDosPrimosPrimerIterador (0, num)
    where
        esSumaDeDosPrimosPrimerIterador :: (Integer, Integer) -> Bool
        esSumaDeDosPrimosPrimerIterador (x, objetivo)
            | (x >= objetivo) = esSumaDeDosPrimosSegundoIterador (x, 0, objetivo)
            | not (esPrimo x) = esSumaDeDosPrimosPrimerIterador (x+1, objetivo)
            | esSumaDeDosPrimosSegundoIterador (x, 0, objetivo) = True
            | otherwise = esSumaDeDosPrimosPrimerIterador (x+1, objetivo)
        esSumaDeDosPrimosSegundoIterador :: (Integer, Integer, Integer) -> Bool
        esSumaDeDosPrimosSegundoIterador (x, currentY, objetivo)
            | currentY >= objetivo = False
            | (esPrimo x) && (esPrimo currentY)
                && ((x + currentY) == objetivo) = True
            | otherwise = esSumaDeDosPrimosSegundoIterador (x, currentY + 1, objetivo)

-- EJ 1
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n
    | (esPar n) && (n > 2) && (esSumaDeDosPrimos n) = True
    | otherwise = False

-- EJ 2
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n
    | n <= 2 = False
    | n == 3 = True
    | (n `mod` 2) /= 0 = verificarConjeturaHasta (n-1)
    | otherwise = (satisfaceGoldbach n) && verificarConjeturaHasta (n-1)

-- EJ 3
descomposicionEnPrimos :: Integer -> (Integer,Integer)
descomposicionEnPrimos n
    | (n <= 2) || ((n `mod` 2) /= 0) = (-1,-1) -- tiene que ser mayor a 2 y par
    | otherwise = descomposicionEnPrimosIteradorEnX (0,n)
    where 
        descomposicionEnPrimosIteradorEnX :: (Integer, Integer) -> (Integer, Integer)
        descomposicionEnPrimosIteradorEnX (x, objetivo)
            | x >= objetivo = (-1,-1)
            | y /= -1 = (x, y)
            | otherwise = descomposicionEnPrimosIteradorEnX(x+1,objetivo)
            where y = descomposicionEnPrimosIteradorEnY(x,objetivo,0)
        descomposicionEnPrimosIteradorEnY :: (Integer, Integer, Integer) -> Integer
        descomposicionEnPrimosIteradorEnY (x, objetivo, currentY)
            | (esPrimo x) && (esPrimo currentY) && ((x + currentY) == objetivo) = currentY
            | (currentY + x) >= objetivo = -1
            | otherwise = descomposicionEnPrimosIteradorEnY (x,objetivo,currentY+1)


numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n
    | (n <= 2) || ((n `mod` 2) /= 0) = -1 -- tiene que ser mayor a 2 y par
    | otherwise = descomposicionEnPrimosIteradorEnX (0,n)
    where 
        descomposicionEnPrimosIteradorEnX :: (Integer, Integer) -> Integer
        descomposicionEnPrimosIteradorEnX (x, objetivo)
            | x >= objetivo = 0
            | esPrimo x = descomposicionEnPrimosIteradorEnY (x,objetivo,1) + descomposicionEnPrimosIteradorEnX(x+1,objetivo)
            | otherwise = descomposicionEnPrimosIteradorEnX(x+1,objetivo)
        descomposicionEnPrimosIteradorEnY :: (Integer, Integer, Integer) -> Integer
        descomposicionEnPrimosIteradorEnY (x, objetivo, currentY)
            | (esPrimo x) && (esPrimo currentY) && ((x + currentY) == objetivo) = 1
            | (currentY + x) >= objetivo = 0
            | otherwise = descomposicionEnPrimosIteradorEnY (x,objetivo,currentY+1)