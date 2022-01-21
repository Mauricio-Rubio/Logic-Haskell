data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord) 

data Formula = Prop Var
    |Neg Formula
    |Formula :&: Formula
    |Formula :|: Formula
    |Formula :=>: Formula
    |Formula :<=>: Formula deriving (Show, Eq, Ord)
infix 9 :&:
infix 9 :|:
infix 7 :=>:
infix 8 :<=>:
-------------------------------------------------------------
-- Ejercicio 1.-Negación
negar :: Formula -> Formula
negar (Neg f) = f
negar (Prop v) = Neg (Prop v)
negar (p :&: q) = (negar p) :|: (negar q)
negar (p :|: q) = (negar p) :&: (negar q) 
negar (p :=>: q) = (p) :&: (negar q) 
negar (p :<=>: q) = negar ((p :=>: q) :&: (q :=>: p))
-------------------------------------------------------------

-------------------------------------------------------------
-- Ejercicio 2.-Variables de la Fórmula (Falta quitar variables repetidas)
variablesAux :: Formula -> [Var]
variablesAux (Prop a) = [a]
variablesAux (p :&: q) = (variablesAux p) ++ (variablesAux q)
variablesAux (p :|: q) = (variablesAux p) ++ (variablesAux q)
variablesAux (p :=>: q) = (variablesAux p) ++ (variablesAux q)
variablesAux (p :<=>: q) = (variablesAux p) ++ (variablesAux q)
-------------------------------------------------------------

-------------------------------------------------------------
-- Ejercicio 3.-Equivalencia
equivalencia :: Formula -> Formula
equivalencia (Neg p) = equivalencia (negar p)
equivalencia (Prop a) = (Prop a)
equivalencia (p :=>: q) = (equivalencia p) :&: (negar (equivalencia q))
equivalencia (p :<=>: q) = (equivalencia (p :=>: q)) :&: (equivalencia (q :=>: p)) 
equivalencia (p :|: q) = (equivalencia p :|: equivalencia q)
equivalencia (p :&: q) = (equivalencia p :&: equivalencia q)
-------------------------------------------------------------

-------------------------------------------------------------
interpretacionVariable :: Var -> [(Var,Bool)] -> Bool
interpretacionVariable v ((x,b) : xs) = if v == 
    x then b else interpretacionVariable v xs

negacion :: Bool -> Bool 
negacion True = False
negacion False = True 

conjuncion :: Bool -> Bool -> Bool 
conjuncion True True = True
conjuncion x y = False

disyuncion :: Bool -> Bool -> Bool 
disyuncion False False = False
disyuncion x y = True

implicacion :: Bool -> Bool -> Bool 
implicacion True False = False
implicacion x y = True

dimplicacion :: Bool -> Bool -> Bool 
dimplicacion True True = True
dimplicacion x y = False
-------------------------------------------------------------
-- Ejercicio 4.- Interpretación
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion (Prop v) xs = interpretacionVariable v xs
interpretacion (Neg f) xs = negacion (interpretacion f xs)
interpretacion (p :&: q) xs = conjuncion (interpretacion p xs) (interpretacion q xs)
interpretacion (p :|: q) xs = disyuncion (interpretacion p xs) (interpretacion q xs)
interpretacion (p :=>: q) xs = implicacion (interpretacion p xs) (interpretacion q xs)
interpretacion (p :=>: q) xs = dimplicacion (interpretacion p xs) (interpretacion q xs)
-------------------------------------------------------------
-------------------------------------------------------------
agregarValor :: a -> [[a]] -> [[a]]
agregarValor x [] = []
agregarValor x (y:ys) = ((x:y):agregarValor x ys)
-------------------------------------------------------------
-- Ejercicio 5.- Tabla de verdad
tablaVerdad :: [Var] -> [[(Var,Bool)]]
tablaVerdad [x] = [[(x,True)],[(x,False )]]
tablaVerdad (x:xs) = (agregarValor (x,True) (tablaVerdad xs)) ++ (agregarValor (x,False) (tablaVerdad xs))
-------------------------------------------------------------

conjuncionLista1 :: [Bool] -> Bool
conjuncionLista1 [b] = b
conjuncionLista1 (x:xs) = conjuncion x (conjuncionLista1 xs)

conjuncionLista2 :: [Bool] -> Bool
conjuncionLista2 [b] = b
conjuncionLista2 (x:xs) = if x then conjuncionLista2 xs else x

contiene ::Eq a => a -> [a] -> Bool
contiene x [] = False
contiene x (y:ys) = if x == y then True else contiene x ys

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = if contiene x xs then conjunto xs else (x:conjunto xs)
 
--variablesAux :: Formula -> [Var]
--variablesAux f = []

variables :: Formula -> [Var]
variables f = conjunto (variablesAux f)

interpretacionLista :: Formula -> [[(Var,Bool)]] -> [Bool]
interpretacionLista f [] = []
interpretacionLista f (x:xs) = (interpretacion f x : interpretacionLista f xs)

tautologia :: Formula -> Bool
tautologia f = conjuncionLista1 (interpretacionLista f (tablaVerdad (variables f)))
