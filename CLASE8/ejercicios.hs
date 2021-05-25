type Set a = [a]

reshape :: Set a -> Set (Set a)
reshape [] = []
reshape (x:xs) = ([x]: reshape xs)

agregarAlFinalDelSet :: Int -> Set Int -> Set Int
agregarAlFinalDelSet n [] = [n]
agregarAlFinalDelSet n (x:xs) = (x:agregarAlFinalDelSet n xs)

agregarATodos :: Set (Set Int) -> Int -> Set (Set Int)
agregarATodos [] _ = []
agregarATodos (x:xs) n = (agregarAlFinalDelSet n x : agregarATodos xs n)

productoCartesianoRNxR :: Set(Set Int) -> Set Int -> Set(Set Int)
productoCartesianoRNxR _ [] = []
productoCartesianoRNxR rn (r:rs) = (agregarATodos rn r) ++ productoCartesianoRNxR rn rs

bolitasEnCajas :: Int -> Int -> Set (Set Int)
bolitasEnCajas 1 b = reshape [1..b]
bolitasEnCajas c b = productoCartesianoRNxR (bolitasEnCajas (c-1) b) [1..b]

pertenece :: (Eq a)=> a -> Set a -> Bool
pertenece _ [] = False
pertenece n (x:xs)
    | n == x = True
    | otherwise = pertenece n xs

filtrarSoloSetsConPrimeraCajaNoVacia :: Set(Set Int) -> Set(Set Int)
filtrarSoloSetsConPrimeraCajaNoVacia [] = []
filtrarSoloSetsConPrimeraCajaNoVacia (x:xs) 
    | pertenece 1 x = (x:filtrarSoloSetsConPrimeraCajaNoVacia xs)
    | otherwise = filtrarSoloSetsConPrimeraCajaNoVacia xs

largo :: Set a -> Int
largo [] = 0
largo (x:xs) = 1 + largo xs

combinacionesBolitasEnCajasPrimeraCajaNuncaVacia :: Int -> Int -> Set (Set Int)
combinacionesBolitasEnCajasPrimeraCajaNuncaVacia b c = filtrarSoloSetsConPrimeraCajaNoVacia(bolitasEnCajas b c)


remove :: (Eq a) => a -> Set a -> Set a
remove _ [] = []
remove n (x:xs)
    | n == x = xs
    | otherwise = (x:remove n xs)

agregarAlFinal :: (Eq a) => Set a -> a -> Set a
agregarAlFinal [] n = [n]
agregarAlFinal (x:xs) n = (x:agregarAlFinal xs n)

agregarATodosAlFinal :: (Eq a) => Set (Set a) -> a -> Set(Set a)
agregarATodosAlFinal [] _ = []
agregarATodosAlFinal (x:xs) n = ((agregarAlFinal x n):agregarATodosAlFinal xs n)

permutaciones :: (Eq a) => Set a -> Set (Set a)
permutaciones [] = []
permutaciones [x] = [[x]]
permutaciones p = permAux p p
    where 
        permAux :: (Eq a) => Set a -> Set a -> Set (Set a)
        permAux _ [] = []
        permAux l (n:ns) = (agregarATodosAlFinal (permutaciones (remove n l)) n) ++ permAux l (ns)

filtrarPorListaOrdenada :: Set (Set Int) -> Set (Set Int)
filtrarPorListaOrdenada [] = []
filtrarPorListaOrdenada (x:xs)
    | estaOrdenadaDecreciente x || estaOrdenadaCreciente x = (x: filtrarPorListaOrdenada xs)
    | otherwise = filtrarPorListaOrdenada xs

hayProximoValor :: Set(a) -> Bool
hayProximoValor [] = False
hayProximoValor (x:xs) = True

estaOrdenadaCreciente :: Set Int -> Bool
estaOrdenadaCreciente [] = True
estaOrdenadaCreciente (x:xs)
    | not (hayProximoValor xs) = True
    | (head xs) >= x = estaOrdenadaCreciente xs
    | otherwise = False

estaOrdenadaDecreciente :: Set Int -> Bool
estaOrdenadaDecreciente [] = True
estaOrdenadaDecreciente (x:xs)
    | not (hayProximoValor xs) = True
    | (head xs) <= x = estaOrdenadaDecreciente xs
    | otherwise = False

listasOrdenadasDe1HastaN :: Int -> Int -> Set (Set Int)
listasOrdenadasDe1HastaN n k = filtrarPorListaOrdenada (bolitasEnCajas n k)

repetir :: (Eq a) => Int -> a -> Set (a)
repetir 0 _ = []
repetir n a = (a:repetir (n-1) a)

iguales :: (Eq a) => Set a -> Set a -> Bool
iguales [] [] = True
iguales [] _ = False
iguales _ [] = False
iguales (a:as) (b:bs)
    | a == b = iguales as bs
    | otherwise = False

incluidoSet :: (Eq a) => Set a -> Set (Set a) -> Bool
incluidoSet _ [] = False
incluidoSet e (x:xs)
    | iguales e x = True
    | otherwise = incluidoSet e xs

removerRepeticiones :: (Eq a) => Set (Set a) -> Set (Set a)
removerRepeticiones [] = []
removerRepeticiones (x:xs)
    | incluidoSet x xs = removerRepeticiones xs
    | otherwise = (x: removerRepeticiones xs)


sucesionNaMb :: Int -> Int -> Set (Set Char)
sucesionNaMb n m = removerRepeticiones (permutaciones ((repetir n 'a')++ (repetir m 'b')))

sucesionNaMbGc :: Int -> Int -> Int -> Set (Set Char)
sucesionNaMbGc n m g = removerRepeticiones (permutaciones ((repetir n 'a')++ (repetir m 'b')++ repetir g 'c' ))

subconjuntos :: Set Int -> Int -> Set (Set Int)
subconjuntos [] _ = []
subconjuntos (x:xs) length
    | largo (x:xs) < length = []
    | largo (x:xs) == length = [(x:xs)]
    | length == 1 = reshape (x:xs)
    | otherwise = agregarATodos ( subconjuntos xs (length - 1) ) x ++ subconjuntos xs length