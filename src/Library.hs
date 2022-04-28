module Library where
import PdePreludat


doble nro = nro * 2

esBisiesto :: Number -> Bool
esBisiesto anio = esMultiploDe 400 anio || esMultiploDe 4 anio && not (esMultiploDe 100 anio)

esMultiploDe :: Number -> Number -> Bool
esMultiploDe divisor dividendo = mod dividendo divisor == 0

data Tomate = UnTomate {
    caracteristicas:: [String],
    peso:: Number 
} deriving (Show,Eq)

tomate1 = UnTomate ["grande", "lindo","rojo"] 50
tomate2 = UnTomate ["rojo","maduro"] 60
tomate3 = UnTomate [] 121

algunosTomates = [tomate1,tomate2,tomate3]
-- Primer ejemplo de necesidad de crear mi función de orden superior:

-- Accesorio de corte
cortar :: Tomate -> Number
cortar tomate = div (peso tomate) 10 

-- Máquina cortadora (v1)
-- cortadora tomates = sum (map cortar tomates) - length tomates
-- Máquina cortadora (v2)
cortadora :: [ Tomate ] -> Number
cortadora tomates = multiprocesadora cortar tomates 


-- Accesorio de rayadura
rayar :: Tomate -> Number
rayar tomate  = min (peso tomate) 100

-- Máquina rayadora (v1)
-- rayadora tomates = sum (map rayar tomates) - length tomates
-- Máquina rayadora (v2)
rayadora :: [ Tomate ] -> Number
rayadora tomates = multiprocesadora rayar tomates


-- Máquina genérica que recibe el accesorio ¡en lugar de las máquinas anteriores!
-- Construyo mi propia función de orden superior
multiprocesadora:: (Tomate -> Number)  -> [Tomate] -> Number
multiprocesadora accesorio tomates = sum (map accesorio tomates ) - length tomates

-- OTRO EJEMPLO de necesidad de construir mi función de orden superior:
maduro :: Tomate -> Bool
maduro tomate = elem "maduro" (caracteristicas tomate)

-- Laburo manual con cuchillo (v1)
-- trabajaConCuchillo tomate 
-- | maduro tomate = cortar tomate 
-- | otherwise = 1
-- Laburo manual con cuchillo (v2)
trabajaConCuchillo:: Tomate -> Number
trabajaConCuchillo tomate = trabajaCon cortar tomate

-- Laburo manual con rayador
trabajaConRayador:: Tomate -> Number
trabajaConRayador tomate 
 | maduro tomate = rayar tomate 
 | otherwise = 1
 
-- Laburo manual genérico (¡en lugar de las anteriores!)
-- Construyo mi propia función de orden superior
trabajaCon :: (Tomate -> Number) -> Tomate ->  Number
trabajaCon accesorio tomate 
  | maduro tomate = accesorio tomate
  | otherwise = 1


-- Los cocineros
data Cocinero = UnCocinero {
  nombre :: String,
  accesorioPreferido:: Tomate -> Number
}

german = UnCocinero "German" cortar
donato = UnCocinero "Donato" rayar

cocinaMucho:: Cocinero -> [Tomate] -> Bool
cocinaMucho cocinero tomates = (multiprocesadora (accesorioPreferido cocinero) tomates) > 50


losDosPrimeros palabra = take 2 palabra
-------------------------------
olla :: String -> Tomate -> Tomate
olla liquido tomate = 
    UnTomate ("cocido":caracteristicas tomate) 
             (peso tomate * incremento liquido)

incremento "aceite" = 1.1
incremento "agua" = 1.2

algo f = f 3

freir = olla "aceite"

esPar = even

mitad = (/2)

anioActual = 2022
--hervir tomate = olla "agua" tomate

