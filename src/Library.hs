module Library where
import PdePreludat

esBisiesto :: Number -> Bool
esBisiesto anio = esMultiploDe anio 400 || esMultiploDe anio 4 && not (esMultiploDe anio 100)

esMultiploDe :: Number -> Number -> Bool
esMultiploDe divisor dividendo = mod dividendo divisor == 0
