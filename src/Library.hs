module Library where
import PdePreludat

esBisiesto :: Number -> Bool
esBisiesto anio = esMultiploDe 400 anio || esMultiploDe 4 anio && not (esMultiploDe 100 anio)

esMultiploDe :: Number -> Number -> Bool
esMultiploDe divisor dividendo = mod dividendo divisor == 0
