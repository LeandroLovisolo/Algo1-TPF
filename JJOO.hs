module JJOO (JJOO(..), nuevoJ, anioJ, atletasJ, cantDiasJ, cronogramaJ,
             jornadaActualJ, dePaseoJ, medalleroJ,
             boicotPorDisciplinaJ, losMasFracasadosJ, liuSongJ,
             stevenBradburyJ, uyOrdenadoAsiHayUnPatronJ, sequiaOlimpicaJ,
             transcurrirDiaJ,)
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

dePaseoJ :: JJOO -> [Atleta]
dePaseoJ juegos = auxDePaseoJ juegos (atletasJ juegos)

auxExisteAtletaConCia :: [Atleta] -> Int -> Bool
auxExisteAtletaConCia [] _ = False
auxExisteAtletaConCia (atle:atletas) cia | ((ciaNumberA atle) == cia) = True
										 | otherwise = auxExisteAtletaConCia atletas cia

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
-- falta armarlo, me trabe despues lo sigo
medalleroJ                = undefined
--medalleroJ ::JJOO -> [(Pais, [Int])]

--No se puede usar Finalizar! constructor de competencia
--auxPodio :: Competencia->[Atleta]
--auxPodio (Finalizar ciaNum dopping compe) | length (rankingC (sancionarTrampososC (Finalizar ciaNum dopping compe)))<3 = []
--										  | length (rankingC (sancionarTrampososC (Finalizar ciaNum dopping compe))) >= 3 = head (rankingC (sancionarTrampososC (Finalizar ciaNum dopping compe))) : head (tail (rankingC (sancionarTrampososC (Finalizar ciaNum dopping compe)))) : head (tail (tail (rankingC (sancionarTrampososC (Finalizar ciaNum dopping compe))))) : []
										  -- si el ranking tiene mas de 3 atletas me da la lista de los primeros 3

--auxpaisPodio :: [Atleta]->[Pais]
--auxpaisPodio [] = []
--auxpaisPodio (atleta:atletas) = nacionalidadA atleta : paisPodio atletas

--auxPaisMedallas :: [[Pais]] ->(Pais,[Int])-> (Pais,[Int])
--auxPaisMedallas [] (p, [a,b,c]) = (p,[a,b,c])
--auxPaisMedallas (pais:paises) (p, [a,b,c]) | (pais!!0)==fst (p, [a,b,c]) = auxPaisMedallas paises (p,[a+1,b,c])
--							               | (pais!!1)==fst (p, [a,b,c]) = auxPaisMedallas paises (p,[a,b+1,c])
--							   			   | (pais!!2)==fst (p, [a,b,c]) = auxPaisMedallas paises (p,[a,b,c+1])

---------------------------------------------------------------------------------------------------------------------------------

boicotPorDisciplinaJ      = undefined
losMasFracasadosJ         = undefined
liuSongJ                  = undefined
stevenBradburyJ           = undefined
uyOrdenadoAsiHayUnPatronJ = undefined
sequiaOlimpicaJ           = undefined
transcurrirDiaJ           = undefined


--Prototipo de transcurrir dia, falta auxCrearRanking y asignar algun al dopping!
--En caso de que la especificacion este mal, porque dice que requiere que no sea el ultimo pero el invariante admite que sea el ultimo dia
auxAumentarDia :: Int -> Int -> Int
auxAumentarDia diaActual maxDias | diaActual < maxDias = diaActual + 1
								 | diaActual == maxDias = diaActual

--auxCrearRanking :: [Atleta] -> Categoria -> [Atleta]


--auxFinalizarCompetencias :: [Competencia] -> [Competencia]
--auxFinalizarCompetencias [] = []
--auxFinalizarCompetencias (compe:competencias) = (finalizarC (auxCrearRanking (participantesC compe) (categoriaC compe)) dopping compe) : (auxFinalizarCompetencias competencias)

--transcurrirDiaJ :: JJOO -> JJOO
--transcurrirDiaJ juegos = auxTranscurrirDiaJ juegos (jornadaActualJ juegos)
--auxTranscurrirDiaJ :: JJOO -> Int -> JJOO
--auxTranscurrirDiaJ (J _ _ diaActual) _ = (J _ _ auxAumentarDia (diaActual+1))
--auxTranscurrirDiaJ (NuevoDia competencias juegos) 0 = (NuevoDia (auxFinalizarCompetencias competencias) juegos) (-1)
--auxTranscurrirDiaJ (NuevoDia _ juegos) i = auxTranscurrirDiaJ juegos (i-1)