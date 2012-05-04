module Atleta (Atleta, nuevoA, nombreA, sexoA, anioNacimientoA,
               nacionalidadA, ciaNumberA, deportesA, capacidadA,
               entrenarDeporteA)
where

import Tipos

data Atleta = A String Sexo Int Pais Int [(Deporte, Int)] deriving (Show)