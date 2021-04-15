factorial :: Integer -> Integer
factorial x 
    | x == 0 = 1
    | otherwise = (factorial (x-1)) * x


sumatoria :: Int -> Int
sumatoria n
    | n <= 0 = 0
    | otherwise = sumatoria (n - 1) + n

f1 :: Int -> Int
f1 n 
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = 2^n + f1 (n-1)

f2 :: Int -> Int -> Int
f2 n q
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = q^n + f2 (n-1) q

f3 :: Int -> Int -> Int
f3 n q
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = ejecutarSerie (1, n*2, q)
        where 
            ejecutarSerie :: (Int, Int, Int) -> Int
            ejecutarSerie (orig, dest, base)
                | orig == dest = base^orig
                | otherwise = base ^ orig + ejecutarSerie ((orig+1), dest, base)

f4 :: Int -> Int -> Int
f4 n q
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = ejecutarSerie (n, n*2, q)
        where 
            ejecutarSerie :: (Int, Int, Int) -> Int
            ejecutarSerie (orig, dest, base)
                | orig == dest = base^orig
                | otherwise = base ^ orig + ejecutarSerie ((orig+1), dest, base)

eApprox :: Integer -> Float
eApprox num
    | num <= 0 = 1
    | otherwise = (1.0/(fromIntegral (factorial num))) + eApprox (num - 1)



primerSumaDoble :: Int -> Int -> Int
primerSumaDoble n m
    | n < 1 || m < 1 = 0
    | otherwise = iteradorEnBase n 1 m
    where 
        iteradorEnBase :: Int -> Int -> Int -> Int
        iteradorEnBase maxN currentN maxExp 
            | maxN < currentN = 0
            | maxN == currentN = iteradorEnExponencial currentN 1 maxExp
            | otherwise = (iteradorEnExponencial currentN 1 maxExp) + iteradorEnBase maxN (currentN + 1) maxExp
        iteradorEnExponencial :: Int -> Int -> Int -> Int
        iteradorEnExponencial base exp0 expDest
            | exp0 >= expDest = base^exp0
            | otherwise = base^exp0 + iteradorEnExponencial base (exp0 + 1) expDest

sumaPotencias :: (Int, Int, Int) -> Int
sumaPotencias (q, n, m)
    | n < 1 || m < 1 = 0
    | otherwise = primerIterador (q,n,m,1)
        where
            primerIterador :: (Int, Int, Int, Int) -> Int
            primerIterador (base, maxA, maxB, currentA)
                | maxA < currentA = 0
                | otherwise = segundoIterador(base, currentA, maxB, 1) + primerIterador (base, maxA, maxB, currentA + 1)
            segundoIterador :: (Int, Int, Int, Int) -> Int
            segundoIterador (base, currentA, maxB, currentB)
                | maxB < currentB = 0
                | otherwise = base^(currentA + currentB) + segundoIterador (base, currentA,maxB, currentB+1)

sumaRacionales :: (Int, Int) -> Float
sumaRacionales (maxNum, maxDen)
    | maxNum < 1 || maxDen < 1 = 0
    | otherwise = primerIterador (1,maxNum,maxDen)
        where
            primerIterador :: (Int, Int, Int) -> Float
            primerIterador (currentNum, maxNum, maxDen)
                | maxNum < currentNum = 0
                | otherwise = segundoIterador(1, maxDen, currentNum) + primerIterador (currentNum + 1, maxNum, maxDen)
            segundoIterador :: (Int, Int, Int) -> Float
            segundoIterador (currentDen, maxDen, currentNum)
                | maxDen < currentDen = 0
                | otherwise = ((fromIntegral currentNum)/(fromIntegral currentDen)) + segundoIterador (currentDen + 1, maxDen, currentNum)

g1 :: (Int, Int) -> Int
g1  (base, maxExp)
    | base == 0 = 0
    | otherwise = primerIterador base base maxExp
    where 
        primerIterador :: Int -> Int -> Int -> Int
        primerIterador base currentExp maxExp
            | maxExp < currentExp = 0
            | otherwise = base^currentExp + primerIterador base (currentExp + 1) maxExp

g2 :: Int -> Int
g2  base
    | base < 1 = 0
    | otherwise = primerIterador (1, base, base)
    where 
        primerIterador :: (Int, Int, Int) -> Int
        primerIterador (currentBase, maxBase, maxExp)
            | maxBase < currentBase = 0
            | otherwise = segundoIterador (currentBase, 1, maxExp) + primerIterador (currentBase + 1, maxBase, maxExp)
        segundoIterador :: (Int, Int, Int) -> Int
        segundoIterador (base, currentExp, maxExp)
            | currentExp > maxExp = 0
            | otherwise = segundoIterador (base, currentExp + 1, maxExp) + base^currentExp

g3 :: Int -> Int
g3 maxExp
    | maxExp < 1 = 0
    | otherwise = primerIterador 1 maxExp
    where
        primerIterador :: Int -> Int -> Int
        primerIterador currentExp maxExp
            | maxExp < currentExp = 0
            | currentExp `mod` 2 == 1 = primerIterador (currentExp + 1) maxExp
            | otherwise = (primerIterador (currentExp + 1) maxExp) + 2^currentExp

sonTodosLosDigitosIguales :: Int -> Bool
sonTodosLosDigitosIguales num 
    | num == 0 = True
    | num < 0 = sonTodosLosDigitosIguales ((-1)*num)
    | otherwise = validarDigitos num (num `mod` 10)
    where
        validarDigitos :: Int -> Int -> Bool
        validarDigitos raw n
            | ((raw `mod` 10) == n) && (raw `div` 10) == 0 = True
            | ((raw `mod` 10) == n) && (raw `div` 10) /= 0 = validarDigitos (raw `div` 10)  n
            | otherwise = False

g4 :: Int -> Int
g4 topNum
    | topNum < 1 = 0
    | otherwise = primerIterador 1 topNum
    where
        primerIterador :: Int -> Int -> Int
        primerIterador currentNum topNum
            | currentNum > topNum = 0
            | sonTodosLosDigitosIguales currentNum = (primerIterador (currentNum+1) topNum) + currentNum
            | otherwise = primerIterador (currentNum + 1) topNum