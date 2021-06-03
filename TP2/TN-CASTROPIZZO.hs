-- Representa la cantidad de "montoncitos" de piedras.
-- NINGÚN ELEMENTO DEL ARRAY PUEDE SER 0
type Posicion = [Int]

-- (posición de la pila, cantidad de piedras a quitar)
-- posición 1 = Primera pila
type Jugada = (Int, Int)

-- Funciones utiles
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--Ejercicio 1
jugar :: Posicion -> Jugada -> Posicion
jugar (x:xs) (p, n) 
    | p == 1 = (removerDelMonton x n) ++ xs
    | otherwise = x : jugar xs (p-1, n)

removerDelMonton :: Int -> Int -> Posicion
removerDelMonton piedrasActuales piedrasARemover
    | piedrasActuales > piedrasARemover = [piedrasActuales - piedrasARemover]
    | otherwise = []

--Ejercicio 2
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas xs = posiblesJugadasConIndiceBase xs 1

posiblesJugadasConIndiceBase :: Posicion -> Int -> [Jugada]
posiblesJugadasConIndiceBase [] _ = []
posiblesJugadasConIndiceBase (x:xs) indice = posiblesJugadasSobreUnMonton indice x ++ posiblesJugadasConIndiceBase xs (indice + 1)

posiblesJugadasSobreUnMonton :: Int -> Int -> [Jugada]
posiblesJugadasSobreUnMonton indiceMonton 1 = [(indiceMonton,1)]
posiblesJugadasSobreUnMonton indiceMonton valorActual = (indiceMonton,valorActual): posiblesJugadasSobreUnMonton indiceMonton (valorActual - 1)

-- Ejercicio 3
{-
Para que una posicion sea ganadora, tiene que haber una jugada que
genere una posición (posicion perdedora) que implique que todos los movimientos posibles
generen posiciones que tengan un solo elemento (que sea la jugada final)

Parafraseo
La posición ganadora tiene una posible jugada que
crea una posición que hace que todas las posibles jugadas
generen una posición final o sean posiciones ganadoras
-}

esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora posicion = hayJugadaGanadora posicion (posiblesJugadas posicion)

hayJugadaGanadora :: Posicion -> [Jugada] -> Bool
hayJugadaGanadora _ [] = False
hayJugadaGanadora posicion (x:xs) = (esJugadaGanadora posicion x) || (hayJugadaGanadora posicion xs)

esJugadaGanadora :: Posicion -> Jugada -> Bool
esJugadaGanadora pos jug = esPosicionPerdedora (jugar pos jug)

esPosicionPerdedora :: Posicion -> Bool
esPosicionPerdedora posicion = sonTodasLasJugadasPerdedoras posicion (posiblesJugadas posicion)

sonTodasLasJugadasPerdedoras :: Posicion -> [Jugada] -> Bool
sonTodasLasJugadasPerdedoras _ [] = True
sonTodasLasJugadasPerdedoras posicion (x:xs) = 
    (esPosicionFinal siguientePosiblePosicion || esPosicionGanadora siguientePosiblePosicion) 
    && sonTodasLasJugadasPerdedoras posicion xs
    where siguientePosiblePosicion = jugar posicion x

esPosicionFinal :: Posicion -> Bool
esPosicionFinal [x] = True
esPosicionFinal _ = False

-- Ejercicio 4
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora posicion = buscarJugadaGanadora posicion (posiblesJugadas posicion)

buscarJugadaGanadora :: Posicion -> [Jugada] -> Jugada
buscarJugadaGanadora _ [] = (0,0) -- solo con motivos de testing y evitar un loop infinito
buscarJugadaGanadora posicion (x:xs) 
    | esJugadaGanadora posicion x = x
    | otherwise = buscarJugadaGanadora posicion xs

-- Ejercicio 5
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras posicion = longitud (buscarJugadasGanadoras posicion (posiblesJugadas posicion))

buscarJugadasGanadoras :: Posicion -> [Jugada] -> [Jugada]
buscarJugadasGanadoras _ [] = []
buscarJugadasGanadoras posicion (x:xs)
    | esJugadaGanadora posicion x = x:buscarJugadasGanadoras posicion xs
    | otherwise = buscarJugadasGanadoras posicion xs