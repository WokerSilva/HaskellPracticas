--Practica Dos -- Estructuras Discretas

--Ejercicio 1
-- La función debe calcular el MCD de dos números enteros positivos.Puedes asumir
--que la función siempre recibirá enteros positivos
mcd :: Int -> Int -> Int
mcd a 0 = a           
mcd a b = mcd b (a `mod` b)


-----------------------------------------------------------------------------
--Ejercicio 2
-- La función debe calcular el MCM de dos n ́umeros enteros positivos.Puedes
--asumir que la función siempre recibir ́a enteros positivos
--Función Auxiliar
multiplosDe :: Int -> [Int]
multiplosDe x = map (*x) [1,2..]
--Funcion Auxiliar
comunes :: Ord a => [a] -> [a] -> a
comunes (x:xs) (y:ys)
  | x > y    = comunes (x:xs)    ys
  | x < y     = comunes (xs)    (y:ys)
  |otherwise = x
--Función Principal
mcm :: Int -> Int -> Int
mcm x y = comunes (multiplosDe x) (multiplosDe y)


-------------------------------------------
--Ejercicio 3
--  lista.Función recursiva que calcula la longitud de una lista.
longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs


------------------------------------------------------------------
--Ejercicio 4
-- La función devuelve el máximo elemento de una lista de tipo num ́erico.
maximo :: Ord a => [a] -> a
maximo [x]       = x
maximo (x:x':xs) = maximo ((if x >= x' then x else x'):xs)


---------------------------------------------------------------------
--Ejercicio 5
--
reversa :: [a] -> [a]
reversa []=[]
reversa (x : xs) = (reversa xs) ++ [x]


------------------------------------------------------------------------------
--Ejercicio 6
-- La función verifica si una lista es pal ́ındromo.

palindromo ::Ord a => [a] -> Bool
palindromo xs = xs == reversa (xs)


-----------------------------------------------------------------------------
--Ejercicio 7
-- La función devuelve una lista con todos los divisores positivos de un número entero positivo.
--Puedes asumir que la función siempre va a recibir enteros positivos.
  --Función Auxiliar 
divisible::Int-> Int-> Bool
divisible x y = (mod x y) == 0
  --Funcion Principal
divisores :: Int -> [Int]
divisores x = [y | y <-[1..x],divisible x y]


---------------------------------------------------------------------------------------
--Ejercicio 8
-- La función debe calcular la diferencia simétrica entre dos listas.
union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, y `notElem` xs]

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion xs ys =
  [x | x <- xs, x `elem` ys]

diferenciaSimetrica :: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica c1 c2 = 
    diferenciaSimetrica (union c1 c2) (interseccion c1 c2)


--------------------------------------------------------------------------------------------
--Ejercicio 9
-- La función debe calcular la multiplicación de las matrices M y N de dimension
--nxm y mxk respectivamente. Para representar una matriz en Haskell usaremos un lista de listas. Puedes asumir
--que siempre vas a recibir matrices válidas para la multiplicación.
--multMatriz ::Num a => [[a]] -> [[a]] -> [[a]]


--Ejercicio 10 
--La función debe calcular el conjunto potencia de una lista.
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = let conjuntoPotencia_xs = conjuntoPotencia xs 
                          in conjuntoPotencia_xs ++ [(x:z) | z <- conjuntoPotencia_xs]