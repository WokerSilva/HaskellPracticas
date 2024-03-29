-- Estructuras Discretas 
-- Practica 3

data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)
data Formula = Prop Var | Neg Formula  | Formula :&: Formula | Formula :|: Formula | Formula :=>: Formula | Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixr 7 :=>:
infixl 8 :<=>:

-- Funciones Auixiliares --

valor :: Var -> [(Var,Bool)] -> Bool
valor a [(x,b)] = b
valor a ((x,b):xs) = if (a == x)
                      then b
                      else valor a xs

negar :: Bool -> Bool
negar True = False
negar False = True

conj :: Bool -> Bool -> Bool
conj True True = True
conj x y = False

disj :: Bool -> Bool -> Bool
disj False False = False
disj x y = True

impl :: Bool -> Bool -> Bool
impl True False = False
impl x y = True

equi :: Bool -> Bool -> Bool
equi True True = True
equi False False = True
equi x y = False

union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, y `notElem` xs]

comV :: Var -> [[(Var,Bool)]] -> [[(Var,Bool)]]
comV p [] = []
comV p (x:xs) = (((p,True):x):(comV p xs))

comF :: Var -> [[(Var,Bool)]] -> [[(Var,Bool)]]
comF p [] = []
comF p (x:xs) = (((p,False):x):(comF p xs))

tablaAux :: [Var] -> [[(Var,Bool)]] -> [[(Var,Bool)]]
tablaAux [] xs = xs
tablaAux (x:xs) ys = tablaAux xs((comV x ys) ++ (comF x ys))

mediador :: Formula -> [[(Var,Bool)]] -> Bool
mediador f [] = True
mediador f (x:xs) = if  interpretacion f x
                  then mediador f xs
                  else False

    -- PRACTICA  3--

--1 Negación
negacion :: Formula -> Formula
negacion = Neg

--2 Variables de la formula 
variables :: Formula -> [Var]
variables (Prop a) = [a]
variables (Neg f)  = variables f 
variables (f :&: g) = variables f `union` variables g 
variables (f :|: g) = variables f `union` variables g 
variables (f :=>: g) = variables f `union` variables g 
variables (f :<=>: g) = variables f `union` variables g

-- 3 Equivalencia
equivalencia :: Formula -> Formula
equivalencia (Prop p) = Prop p 
equivalencia (Neg f) = Neg f
equivalencia (f :=>: g) = (Neg f) :|: (equivalencia g)
equivalencia (f :<=>: g) = ((Neg f) :|: (equivalencia g)) :&: ((Neg f) :|: (equivalencia g))

--4 Intepretación 
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion (Prop a)   xs = valor a xs
interpretacion (Neg f)    xs = negar (interpretacion f xs)
interpretacion (f :&: g)  xs = (interpretacion f xs) && (interpretacion g xs)
interpretacion (f :|: g)  xs = (interpretacion f xs) || (interpretacion g xs)
interpretacion (f :=>: g) xs = impl (interpretacion f xs) (interpretacion g xs)
interpretacion (f :<=>: g)xs = equi (interpretacion f xs) (interpretacion g xs)

--5 Tablas de Verdad 
tablaVerdad :: [Var] -> [[(Var,Bool)]]
tablaVerdad [] = []
tablaVerdad (x:xs) = tablaAux xs [[(x,True)],[(x,False)]]

--6  Tautologia
tautologia :: Formula -> Bool
tautologia f = mediador f (tablaVerdad (variables f))