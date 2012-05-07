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
nuevoJ :: Int -> [Atleta] -> [[Competencia]] -> JJOO
nuevoJ anio atletas [] = (J anio atletas 1)
nuevoJ anio atletas (compe:competencias) = NuevoDia compe (nuevoJ anio atletas competencias)

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

jornadaActualJ :: JJOO -> Int
jornadaActualJ (J _ _ jornadaActual) = jornadaActual
jornadaActualJ (NuevoDia _ juegos) = jornadaActualJ juegos

--Prototipo de dePaseoJ--Error de pattern matching, a ver si alguien lo puede ver ------------------------------------------------
dePaseoJ :: JJOO -> [Atleta]
dePaseoJ juegos = auxDePaseoJ juegos (atletasJ juegos)

auxExisteAtletaConCia :: [Atleta] -> Int -> Bool
auxExisteAtletaConCia [] _ = False
auxExisteAtletaConCia (atle:atletas) cia | ((ciaNumberA atle) == cia) = True

auxSacarAtletas :: [Atleta] -> [Atleta] -> [Atleta]
auxSacarAtletas atletas [] = []
auxSacarAtletas [] atletasParaSacar = []
auxSacarAtletas (atle:atletas) atletasParaSacar | auxExisteAtletaConCia atletasParaSacar (ciaNumberA atle) = auxSacarAtletas atletas atletasParaSacar
												| otherwise = atle : auxSacarAtletas atletas atletasParaSacar
auxDePaseoJ :: JJOO -> [Atleta] -> [Atleta]
auxDePaseoJ (J _ _ _) atles = atles
auxDePaseoJ (NuevoDia (compe:competencias) juegos) atles = auxDePaseoJ (NuevoDia competencias juegos) (auxSacarAtletas atles (participantesC compe))
auxDePaseoJ (NuevoDia [] juegos) atles = auxDePaseoJ juegos atles
-------------------------------------------------------------------------------------------------------------------------------

medalleroJ                = undefined
--medalleroJ ::JJOO -> [(Pais, [Int])]
boicotPorDisciplinaJ      = undefined
losMasFracasadosJ         = undefined
liuSongJ                  = undefined
stevenBradburyJ           = undefined
uyOrdenadoAsiHayUnPatronJ = undefined
sequiaOlimpicaJ           = undefined
transcurrirDiaJ           = undefined