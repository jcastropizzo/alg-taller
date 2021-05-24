sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta num hasta
    | hasta < 1 = 0
    | num < 0 = sumaDivisoresHasta (num*(-1)) hasta
    | otherwise = iterador(num,hasta,2)
    where 
        iterador :: (Int, Int,Int)-> Int
        iterador (num, hasta, currentDiv)
            | currentDiv > num || currentDiv > hasta = 0
            | currentDiv == num = currentDiv
            | num `mod` currentDiv == 0 = currentDiv + iterador (num,hasta,currentDiv + 1)
            | otherwise  = iterador (num,hasta,currentDiv + 1)

sumaDivisores :: Int -> Int
sumaDivisores num = sumaDivisoresHasta num num

esPar :: Int -> Bool
esPar n = ( n `mod` 2) == 0

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

tomarValorMax :: Int -> Int -> Int
tomarValorMax nmin nmax = tomarValorMaxIterator nmin nmax 0
    where
        tomarValorMaxIterator :: Int -> Int -> Int -> Int
        tomarValorMaxIterator nmin nmax maxValue
            | (nmin - 1) == nmax = maxValue
            | maxValue < currentValue = tomarValorMaxIterator nmin (nmax-1) currentValue
            | otherwise = tomarValorMaxIterator nmin (nmax-1) maxValue
                where currentValue = sumaDivisores nmax


tomarValorMin :: Int -> Int -> Int
tomarValorMin nmin nmax = tomarValorMinIterator nmin (nmax-1) (sumaDivisores nmax)
    where
        tomarValorMinIterator :: Int -> Int -> Int -> Int
        tomarValorMinIterator nmin nmax minValue
            | (nmin - 1) == nmax = minValue
            | minValue > currentValue = tomarValorMinIterator nmin (nmax-1) currentValue
            | otherwise = tomarValorMinIterator nmin (nmax-1) minValue
                where currentValue = sumaDivisores nmax

-- primos gemelos: a (lowPrime) y b (highPrime), con b = a + 2
sonPrimosGemelosConLowPrime :: Int -> Bool
sonPrimosGemelosConLowPrime lowPrime = esPrimo lowPrime && esPrimo (lowPrime + 2)

sonPrimosGemelosConHighPrime :: Int -> Bool
sonPrimosGemelosConHighPrime highPrime =  esPrimo highPrime && esPrimo (highPrime - 2)

primosGem :: Int -> Int
primosGem top
    | top < 4 = 0
    | sonPrimosGemelosConHighPrime top = 1 + primosGem (top - 1)
    | otherwise = primosGem (top - 1)
-- primosGem :: Int -> Int
-- primosGem top = 0

proxPrimosGem :: Int -> (Int, Int)
proxPrimosGem possibleLowPrime
    | sonPrimosGemelosConLowPrime (possibleLowPrime + 1) = (possibleLowPrime+1, possibleLowPrime + 3)
    | otherwise = proxPrimosGem (possibleLowPrime + 1)

lotharCollantzProxValor :: Int -> Int
lotharCollantzProxValor an
    | esPar an = an `div` 2
    | otherwise = an * 3 + 1

largoSecuencia :: Int -> Int
largoSecuencia 1 = 0
largoSecuencia n = 1 + largoSecuencia (lotharCollantzProxValor n)

mayorLargoSecuenciaHastaDiezMil :: Int
mayorLargoSecuenciaHastaDiezMil = mayorLargoSecuenciaHasta (10000 - 1)
    
mayorLargoSecuenciaHasta :: Int -> Int
mayorLargoSecuenciaHasta n = mayorLargoSecuenciaHastaIterador n 1
    where 
        mayorLargoSecuenciaHastaIterador :: Int -> Int -> Int
        mayorLargoSecuenciaHastaIterador currentTop possibleMaxA1
            | currentTop <= 0 = possibleMaxA1
            | largoSecuenciaPossibleMaxA1 < largoSecuenciaCurrentTop = mayorLargoSecuenciaHastaIterador (currentTop - 1) currentTop
            | otherwise = mayorLargoSecuenciaHastaIterador (currentTop - 1) possibleMaxA1
            where 
                largoSecuenciaCurrentTop = largoSecuencia currentTop
                largoSecuenciaPossibleMaxA1 = largoSecuencia possibleMaxA1
