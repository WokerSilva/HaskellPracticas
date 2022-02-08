--Practica 1 || Estructuras Discretas || 

--1
--Evaluación de una función cuadrática
-- La función va a recibir 4 parámetros a, b, c, v, donde a, b y c representan a la ecuación
-- ax²+bx+c. La x es el valor con el que vamos a evaluar a dicha ecuación
evaluacionCuadratica :: Float -> Float -> Float -> Float -> Float
evaluacionCuadratica a b c v = ((a*(x^2)) + (x*b) + c)
                              where x = v

--2
--Hipotenusa de un triagulo
-- La función debe calcular la hipotenusa de un triángulo con base b y altura h
hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt((b^2) + (h^2)) 

--3
--Norma de un vector en el plano
-- La función debe calcular la norma del vector (x, y)
normalVertical :: (Float,Float) -> Float
normalVertical (x,y) = sqrt((x^2) + (y^2))

--4
--Comparador
-- La función recibe dos números tipo Float a y b. La función debe devolver:
--  0 si a y b son iguales
--  1 si a es mayor b
-- −1 si a es menor a b.
comparador :: Float -> Float -> Int
comparador a b = if a == b
               then 0
                else if a > b
               then 1
                else -1

--5
--Suma de fracciones
-- La función recibe dos tuplas que representa a las fracciones a/b y c/d
-- La función debe devolver una tupla con el resultado de sumar las fracciones que recibe como parámetros
sumaFraciones :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaFraciones (a,b) (c,d) = if b == d
                            then(a+c, d)
                            else
                            ((a*d)+(b*c), b*d)

--6
-- La Función debe calcular el producto punto entre los vectores (x1, y1) y (x2, y2)
productoPunto :: (Float,Float) -> (Float,Float) -> Float
productoPunto (x,y) (a,b) = ((x*a) + (y*b))

--7
--Función distancia entre dos puntos en el plano
-- La función debe calcular la distancia entre los puntos (x1, y1) y (x2, y2)
distanciaPuntos :: (Float, Float) -> (Float,Float) ->Float
distanciaPuntos (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

--8
--Pendiente de la recta que pasa por dos puntos
-- La función debe calcular la pendiente de la recta que pasa por los puntos (x1, y1) y (x2, y2)
pendienteRecta :: (Float, Float) -> (Float,Float) ->Float
pendienteRecta (x1,y1) (x2,y2) = if (x2-x1)  == 0
                                  then 0
                                else if (y2-y1) == 0
                                  then 0
                                else 
                                  ((y2-y1)/(x2-x1))

--9
--Raices de una ecuación cuadratica 
-- La función recibe como parámetros tres valores de tipo Float a, b y c 
-- representan a la ecuación ax2 + bx + c = 0. El resultado debe ser una tupla con las dos raices
-- cada raíz es una tupla que representa a un número complejo
-- si la raíz no tiene parte imaginaria, la segunda entrada debe ser 0
raicesCuadraticas :: Float -> Float -> Float -> ((Float,Float),(Float,Float))
raicesCuadraticas a b c = (rmas,rmenos)
                        where
                          d = b^2 - (4*(a*c))
                          rmas = if d > 0
                             then (((-b/(2*a))+(sqrt(d)/(2*a)),0))
                              else ((-b/(2*a)),(-b/(2*a))+(sqrt(d*(-1)))/(2*a))

                          rmenos = if d > 0
                              then (((-b/(2*a))-(sqrt(d)/(2*a)),0))
                               else ((-b/(2*a)),(-b/(2*a))-(sqrt(d*(-1)))/(2*a))

--10
--Volumen de una pirámide regular
-- La función recibe como parámetros tres valores de tipo Float l, h y n. 
-- El parámetro l representa el tamaño de los lados de la pirámide, 
-- h es la altura y n es el número de lados de la base.
-- La función debe calcular de volumen de la piramide 
-- representada por los parámetros l, h y n
volumenPiramidal :: Float -> Float -> Float -> Float
volumenPiramidal n l h = (n*l*(apotema)*h)/6
                        where
                        apotema = l/(2*atan(teta*n)/2)
                        teta = (2*pi)/(n/2)