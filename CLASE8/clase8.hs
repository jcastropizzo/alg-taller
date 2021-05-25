type Set a = [a]

factorial :: Int -> Int
factorial 0 = 1
factorial n 
    | n > 0 = n * factorial (n-1)
    | otherwise = (-n) * factorial (-n-1)

comb :: Int -> Int -> Int
comb _ 0 = 1
comb n k
    | n == k = 1
    | otherwise = (comb (n-1) k) + (comb (n-1) (k-1))

agregarAlFinal :: Set Int -> Int -> Set Int
agregarAlFinal [] n = [n]
agregarAlFinal (x:xs) n = (x:agregarAlFinal xs n)

agregarVariacionesAlFinal :: Set Int -> Set Int -> Set (Set Int)
agregarVariacionesAlFinal _ [] = []
agregarVariacionesAlFinal x (y:ys) = ((agregarAlFinal x y): agregarVariacionesAlFinal x ys)

agregarATodosLosSets :: Set (Set Int) -> Set Int -> Set (Set Int)
agregarATodosLosSets [] _ = []
agregarATodosLosSets (x:xs) ns = (agregarVariacionesAlFinal x ns) ++ agregarATodosLosSets xs ns

variaciones :: Set Int -> Int -> Set (Set Int)
variaciones _ 0 = [[]]
variaciones xs n = agregarATodosLosSets (variaciones xs (n-1) ) xs

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn [] n _ = [n]
insertarEn xs n 1 = (n:xs)
insertarEn (x:xs) n indice = (x:insertarEn xs n (indice-1))

reshape :: Set Int -> Set (Set Int)
reshape [] = []
reshape (x:xs) = ([x]: reshape xs)

remove :: Int -> Set Int -> Set Int
remove _ [] = []
remove n (x:xs)
    | n == x = xs
    | otherwise = (x:remove n xs)

agregarATodosAlFinal :: Set (Set Int) -> Int -> Set(Set Int)
agregarATodosAlFinal [] _ = []
agregarATodosAlFinal (x:xs) n = ((agregarAlFinal x n):agregarATodosAlFinal xs n)

permutaciones :: Set Int -> Set (Set Int)
permutaciones [] = []
permutaciones [x] = [[x]]
permutaciones p = permAux p p
    where 
        permAux :: Set Int -> Set Int -> Set (Set Int)
        permAux _ [] = []
        permAux l (n:ns) = (agregarATodosAlFinal (permutaciones (remove n l)) n) ++ permAux l (ns)