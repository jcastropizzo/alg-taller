sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta num hasta
    | hasta < 1 = 0
    | num < 0 = sumaDivisoresHasta (num*(-1)) hasta
    | otherwise = iterador(num,hasta,1)
    where 
        iterador :: (Int, Int,Int)-> Int
        iterador (num, hasta, currentDiv)
            | currentDiv > num || currentDiv > hasta = 0
            | currentDiv == num = currentDiv
            | num `mod` currentDiv == 0 = currentDiv + iterador (num,hasta,currentDiv + 1)
            | otherwise  = iterador (num,hasta,currentDiv + 1)

sumaDivisores :: Int -> Int
sumaDivisores num = sumaDivisoresHasta num num

esPrimo :: Int -> Bool
esPrimo num
    | num < 0 = False
    | num == 0 = True
    | otherwise = esPrimoIterador (num,num)
    where
        esPrimoIterador :: (Int, Int) -> Bool
        esPrimoIterador (baseNum, currentNum)
            | currentNum <= 1 = True -- Es primo, llegó al final del loop
            | baseNum == currentNum = esPrimoIterador (baseNum, currentNum-1)
            | ((baseNum `mod` currentNum) == 0)
                && (baseNum /= currentNum) = False
            | otherwise = esPrimoIterador (baseNum, currentNum-1)

menorDivisor :: Int -> Int
menorDivisor num
    | num < 0 = menorDivisor (-num)
    | num == 1 = 1
    | otherwise = menorDivisorIterador num 2
    where
        menorDivisorIterador :: Int -> Int -> Int 
        menorDivisorIterador num currentPosibleDiv
            | currentPosibleDiv == num = num
            | num `mod` currentPosibleDiv == 0 = currentPosibleDiv
            | otherwise = menorDivisorIterador num (currentPosibleDiv+1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo primeIndex
    | primeIndex < 1 = 0
    | otherwise = primeIterator 1 primeIndex 1
    where
        primeIterator :: Int -> Int -> Int -> Int
        primeIterator currentIndex desiredIndex currentNum
            | (currentIndex < (desiredIndex + 1))
                && esPrimo currentNum = primeIterator (currentIndex + 1) desiredIndex (currentNum + 1)
            | (currentIndex == (desiredIndex + 1))
                && (esPrimo currentNum) = currentNum
            | otherwise = primeIterator currentIndex desiredIndex (currentNum + 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

menorFactDesde :: Int -> Int
menorFactDesde n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = iteratorMenorFactDesde 1 1 n
    where
        iteratorMenorFactDesde :: Int -> Int -> Int -> Int
        iteratorMenorFactDesde currentFact currentIndex objective
            | currentFact >= objective = currentIndex
            | otherwise = iteratorMenorFactDesde (currentFact * (currentIndex + 1)) (currentIndex + 1) objective

mayorFactDesde :: Int -> Int
mayorFactDesde n
    | n < 0 = 0
    | n <= 1 = 1
    | ((factorial (menorFactDesde n)) == n) = menorFactDesde n
    | otherwise = (menorFactDesde n) - 1

esFact :: Int -> Bool
esFact num
    | num < 1 = False
    | otherwise = iteratorIsFact 0 num
    where
        iteratorIsFact :: Int -> Int -> Bool
        iteratorIsFact currentIndex num
            | (factorial currentIndex) == num = True
            | (factorial currentIndex) > num = False
            | otherwise = iteratorIsFact (currentIndex + 1) num

nEsimoFibonacci :: Int -> Int
nEsimoFibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = iteratorNEsimoFibonacci 0 1 2 n
    where 
        iteratorNEsimoFibonacci :: Int -> Int -> Int-> Int -> Int
        iteratorNEsimoFibonacci minusTwo minusOne currentN targetN
            | currentN == targetN = minusOne + minusTwo
            | otherwise = iteratorNEsimoFibonacci minusOne (minusTwo + minusOne) (currentN + 1) targetN

esFibonacci :: Int -> Bool
esFibonacci n = iteratorEsFibonacci 0 n
    where 
        iteratorEsFibonacci ::Int -> Int ->Bool
        iteratorEsFibonacci currentN targetNumber
            | (nEsimoFibonacci currentN) == targetNumber = True
            | (nEsimoFibonacci currentN) > targetNumber = False
            | otherwise = iteratorEsFibonacci (currentN + 1) targetNumber

sumaPrimosHastaN :: Int -> Int
sumaPrimosHastaN n
    | n < 1 = 0
    | otherwise = iteratorSumaPrimosHastaN 0 1 n
    where
        iteratorSumaPrimosHastaN :: Int -> Int -> Int -> Int
        iteratorSumaPrimosHastaN sum currentN n
            | currentN == n = sum + nEsimoPrimo currentN
            | otherwise = iteratorSumaPrimosHastaN (sum + (nEsimoPrimo currentN)) (currentN + 1) n

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos targetNumber = iteratorEsSumaInicialDePrimos 0 targetNumber
    where 
        iteratorEsSumaInicialDePrimos :: Int -> Int -> Bool
        iteratorEsSumaInicialDePrimos currentN targetNumber
            | (sumaPrimosHastaN currentN) < targetNumber = iteratorEsSumaInicialDePrimos (currentN + 1) targetNumber
            | (sumaPrimosHastaN currentN) == targetNumber = True
            | otherwise = False

