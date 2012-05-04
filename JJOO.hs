module JJOO (JJOO, nuevoJ, anioJ, atletasJ, cantDiasJ, cronogramaJ,
             jornadaActualJ, dePaseoJ, medalleroJ,
             boicotPorDisciplinaJ, losMasFracasadosJ, liuSongJ,
             stevenBradburyJ, uyOrdenadoAsiHayUnPatronJ, sequiaOlimpicaJ,
             transcurrirDiaJ)
where

import Tipos
import Atleta
import Competencia

data JJOO = J Int [Atleta] Int
          | NuevoDia [Competencia] JJOO
            deriving (Show)

nuevoJ                    = undefined
anioJ                     = undefined
atletasJ                  = undefined
cantDiasJ                 = undefined
cronogramaJ               = undefined
jornadaActualJ            = undefined
dePaseoJ                  = undefined
medalleroJ                = undefined
boicotPorDisciplinaJ      = undefined
losMasFracasadosJ         = undefined
liuSongJ                  = undefined
stevenBradburyJ           = undefined
uyOrdenadoAsiHayUnPatronJ = undefined
sequiaOlimpicaJ           = undefined
transcurrirDiaJ           = undefined