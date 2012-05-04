module Atleta (Atleta, nuevoA, nombreA, sexoA, anioNacimientoA,
               nacionalidadA, ciaNumberA, deportesA, capacidadA,
               entrenarDeporteA)
where

import Tipos

data Atleta = A String Sexo Int Pais Int [(Deporte, Int)] deriving (Show)

nuevoA           = undefined
nombreA          = undefined
sexoA            = undefined
anioNacimientoA  = undefined
nacionalidadA    = undefined
ciaNumberA       = undefined
deportesA        = undefined
capacidadA       = undefined
entrenarDeporteA = undefined