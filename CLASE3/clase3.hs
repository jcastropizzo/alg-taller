fib :: Int -> Int
fib n 
    | n == 0 = 0
    | n == 1 = 1
    | n > 1 = fib (n-1) + fib (n-2)
    | otherwise = 0

parteEntera :: Float -> Integer
parteEntera num 
    | num <= 1 = 0
    | otherwise = parteEntera(num-1) + 1

esMultiploDe3 :: Int -> Bool
esMultiploDe3 num 
    | num == 0 = False
    | ( restar3HastaNegativo num ) == 0 = True
    | otherwise = False
    where 
        restar3HastaNegativo :: Int -> Int
        restar3HastaNegativo n 
            | n <= 0 = n
            | otherwise = restar3HastaNegativo (n - 3)

sumaImpares :: Int -> Int
sumaImpares num 
    | num == 0 = 0
    | num > 0 = sumaImpares(num - 1) + 2*(num) - 1
    | otherwise = 0

medioFact :: Int -> Int
medioFact num 
    | num <= 0 = 0
    | num <= 2 = num
    | otherwise = medioFact(num - 2) * num

numeroDeDigitos :: Int -> Int
numeroDeDigitos num
    | num <= 0 = 0
    | (num `div` 10) == 0 = 1
    | otherwise = (numeroDeDigitos (num `div` 10)) + 1

sumaNumeroDeDigitos :: Int -> Int
sumaNumeroDeDigitos num
    | num <= 0 = 0
    | (num `div` 10) == 0 = (num `mod` 10)
    | otherwise = (sumaNumeroDeDigitos (num `div` 10)) + (num `mod` 10)

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