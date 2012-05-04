module Competencia (Competencia, nuevaC, categoriaC, participantesC,
                    finalizadaC, rankingC, lesTocoControlAntiDopingC,
                    leDioPositivoC, finalizarC, linfordChristieC,
                    gananLosMasCapacesC, sancionarTrampososC)

where

import Tipos
import Atleta

data Competencia = C Categoria
                 | Participar Atleta Competencia
                 | Finalizar [Int] [(Int, Bool)] Competencia
                   deriving (Show)
