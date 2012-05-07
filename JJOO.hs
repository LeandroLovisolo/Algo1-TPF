module JJOO (JJOO(..), nuevoJ, anioJ, atletasJ, cantDiasJ, cronogramaJ,
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

--Sinceramente no tengo la mas puta idea de el tema de dia actual, no se puede hacer dia actual si J Int [Atleta] Int es uno solo
--nuevoJ :: Int -> [Atleta] -> [[Competencia]] -> JJOO
--nuevoJ anio atletas competencias = auxNuevoJ anio atletas competencias (length competencias)
--auxNuevoJ :: Int -> [Atleta] -> [[Competencia]] ->Int	 -> JJOO
--nuevoJ anio atletas [] _ = J anio atletas cantDias
--nuevoJ anio atletas (compe:competencias) = NuevoDia compe (auxNuevoJ anio atletas competencias cantDias)



nuevoJ                    = undefined

anioJ :: JJOO -> Int
anioJ (J anio _ _) = anio
anioJ (NuevoDia _ juegos) = anioJ juegos

atletasJ :: JJOO -> [Atleta]
atletasJ (J _ atletas _) = atletas
atletasJ (NuevoDia _ juegos) = atletasJ juegos

cantDiasJ :: JJOO -> Int
cantDiasJ (J _ _ _) = 0
cantDiasJ (NuevoDia _ juegos) = 1 + cantDiasJ juegos

cronogramaJ :: JJOO -> Int -> [Competencia]
cronogramaJ (NuevoDia competencias juegos) dias | (dias-1) /= 0 = cronogramaJ juegos (dias-1)
												| otherwise = competencias

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