module JJOO (JJOO(..), nuevoJ, anioJ, atletasJ, cantDiasJ, cronogramaJ,
             jornadaActualJ, dePaseoJ, medalleroJ,
             boicotPorDisciplinaJ, losMasFracasadosJ, liuSongJ,
             stevenBradburyJ, uyOrdenadoAsiHayUnPatronJ, sequiaOlimpicaJ,
             transcurrirDiaJ,auxOrdenar)
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

--cronograma anterior daba los resultados al revez, el 1er dia mostraba las competencias del ultimo NuevoDia
cronogramaJ :: JJOO -> Int -> [Competencia]
cronogramaJ (J _ _ _) _ = []
cronogramaJ (NuevoDia competencias juegos) dias | dias == cantDiasJ juegos+1	 = competencias
												| otherwise = cronogramaJ juegos dias

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

medalleroJ :: JJOO -> [(Pais, [Integer])]
medalleroJ = undefined

-- dado un jjoo y un dia devuelve ctas competencias finalizaron hasta el momento
auxCompetenciasFinalizadas :: JJOO -> Int-> [Competencia]
auxCompetenciasFinalizadas (J _ _ _) _ = []
auxCompetenciasFinalizadas (NuevoDia competencias juegos) 0 = []
auxCompetenciasFinalizadas (NuevoDia competencias juegos) jornadaActual  
														                | jornadaActual == cantDiasJ (NuevoDia competencias juegos) = auxCompetenciasFinalizadasHoy (NuevoDia competencias juegos) ++ auxCompetenciasFinalizadas juegos jornadaActual
														                | jornadaActual > cantDiasJ (NuevoDia competencias juegos) = cronogramaJ (NuevoDia competencias juegos) (cantDiasJ (NuevoDia competencias juegos)) ++ auxCompetenciasFinalizadas juegos jornadaActual
														                | jornadaActual < cantDiasJ (NuevoDia competencias juegos) = auxCompetenciasFinalizadasHoy (NuevoDia competencias juegos) ++ auxCompetenciasFinalizadas juegos jornadaActual

auxCompetenciasFinalizadasHoy :: JJOO->[Competencia]
auxCompetenciasFinalizadasHoy (NuevoDia competencias juegos) = auxCompetencia (cronogramaJ (NuevoDia competencias juegos) (jornadaActualJ (NuevoDia competencias juegos)))

auxCompetencia :: [Competencia]->[Competencia]
auxCompetencia [] = []
auxCompetencia (compe:competencias) | finalizadaC compe = compe : auxCompetencia competencias
					    		    | otherwise = auxCompetencia competencias



--medalleroJ :: JJOO -> [(Pais, [Integer])]
--medalleroJ j = medallero (paisesQueGanaron j)
--    where medallero [] = []
--          medallero (x:xs) = (x, medalleroPorPais x) : medallero xs
--          paisesQueGanaron j = ?
--          medalleroPorPais p = ?
--          medallistas = ?


paisesQueGanaron :: [Competencia] -> [Pais]
paisesQueGanaron xs = sinRepetidos (obtenerPaises ((auxMedallistas xs 0) ++
                                                   (auxMedallistas xs 1) ++
                                                   (auxMedallistas xs 2)))
    where obtenerPaises [] = []
          obtenerPaises (x:xs) = (nacionalidadA x) : obtenerPaises xs


-- Devuelve las competencias finalizadas hasta la jornada actual inclusive. 
competenciasFinalizadas :: JJOO -> [Competencia]
competenciasFinalizadas j = competencias (jornadaActualJ j) j
    where competencias 1 j = soloFinalizadas (cronogramaJ j 1)
          competencias d j = soloFinalizadas (cronogramaJ j d) ++ (competencias (d - 1) j)
          soloFinalizadas [] = []
          soloFinalizadas (c:cs) = if finalizadaC c
                                   then c : soloFinalizadas cs
                                   else soloFinalizadas cs

-- Dada una lista de competencias finalizas y una posición, devuelve
-- todos los atletas que finalizaron en esa posición.
auxMedallistas :: [Competencia] -> Int -> [Atleta]
auxMedallistas [] m = []
auxMedallistas (x:xs) m
	| (length (rankingC x)) <= m = auxMedallistas xs m
	| otherwise                 = (rankingC x !! m) : auxMedallistas xs m

-- Devuelve los elementos de una lista sin repetir.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if (elem (last (x:xs)) (init (x:xs)))
                         then sinRepetidos (init (x:xs))
                         else (sinRepetidos (init (x:xs))) ++ [last (x:xs)]

-- medalleroJ :: JJOO-> [(Pais, [Integer])]
-- medalleroJ (J _ _ _) = []
-- medalleroJ (NuevoDia competencias juegos) | auxPaisPodio (auxMedallistas (auxCompetenciasFinalizadas (NuevoDia competencias juegos) (jornadaActualJ (NuevoDia competencias juegos))) 0) 


auxPaisPodio :: [Atleta]->[Pais]
auxPaisPodio [] = []
auxPaisPodio (atleta:atletas) = nacionalidadA atleta : auxPaisPodio atletas

-- auxPaisMedallas :: [(Pais,[Int])]-> [(Pais,[Int])]
-- auxPaisMedallas [] = []
-- auxPaisMedallas (p:pais:paises) | snd(p!!0)> snd(pais!!0) = auxPaisMedallas (p:paises)
--							    | snd(p!!0)< snd(pais!!0) = auxPaisMedallas (pais:paises)


-- Datos de prueba. Devuelve una lista de competencias finalizadas.
testCompetencias :: [Competencia]
testCompetencias = [(competencia "Futbol" [333, 111, 222, 555, 444, 777, 888, 666]),
                    (competencia "Basket" [111, 555, 333, 444, 888, 222, 666, 777]),
                    (competencia "Volley" [555, 222, 444, 111, 666, 888, 777, 333])]
	where competencia deporte posiciones =
		finalizarC (nuevaC deporte Masculino testAtletas) posiciones []

-- Datos de prueba. Devuelve una lista de atletas entrenados en ciertos deportes.
testAtletas :: [Atleta]
testAtletas = map entrenarDeportes atletas 
  where atletas = [(nuevoA "Abel"     Masculino 18 "Argentina" 111),
                   (nuevoA "Beto"     Masculino 19 "Brasil"    222),
                   (nuevoA "Carlos"   Masculino 20 "Chile"     333),
                   (nuevoA "Daniel"   Masculino 21 "Dinamarca" 444),
                   (nuevoA "Esteban"  Masculino 22 "Ecuador"   555),
                   (nuevoA "Federico" Masculino 22 "Francia"   666),
                   (nuevoA "Gabriel"  Masculino 22 "Grecia"    777),
                   (nuevoA "Horacio"  Masculino 22 "Honduras"  888)]
        deportes = ["Futbol", "Basket", "Volley"]
        entrenarDeportes atleta = foldl entrenarDeporte atleta deportes
        entrenarDeporte atleta deporte = entrenarDeporteA atleta deporte 10

---------------------------------------------------------------------------------------------------------------------------------


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
								 
--Si la lista de atletas inicial no tiene repetidos entonces funciona, si no habri que buscar una forma de sacar los repetidos. 
--El auxOrdenar los ordenar de menor a mayor, para que los ordene de mayor a menor hay que poner un reverse antes de auxOrdenar en el auxCrearRanking
auxCrearRanking :: [Atleta] -> Categoria -> [Atleta]
auxCrearRanking [] cat = []
auxCrearRanking (atle:atletas) cat |elem (fst(cat)) (deportesA atle) = auxOrdenar(atle:(auxCrearRanking atletas cat)) cat
								   |otherwise = auxOrdenar (auxCrearRanking atletas cat) cat
								   
auxOrdenar:: [Atleta]-> Categoria ->[Atleta]
auxOrdenar [] cat =[]
--auxOrdenar (atle:[]) cat = [atle]
auxOrdenar atletas cat = auxOrdenar (auxSacaUnaVez atletas (auxPrimero atletas cat) ++ [(auxPrimero atletas cat)]) cat

auxPrimero :: [Atleta]-> Categoria->Atleta
auxPrimero (atle:[]) cat = atle
auxPrimero (atle:atletas) cat |capacidadA atle (fst(cat)) > capacidadA (head (atletas)) (fst(cat))  = auxPrimero (atle:(tail atletas)) cat --(atletas)
						 	  |otherwise = auxPrimero (atletas) cat

auxSacaUnaVez :: [Atleta]-> Atleta -> [Atleta]
auxSacaUnaVez [] at = []
auxSacaUnaVez (atle:atletas) at |(ciaNumberA atle) == (ciaNumberA at) = auxSacaUnaVez atletas at
								|otherwise = atle : (auxSacaUnaVez atletas at)
reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = (reverso xs) ++ [x]

--auxFinalizarCompetencias :: [Competencia] -> [Competencia]
--auxFinalizarCompetencias [] = []
--auxFinalizarCompetencias (compe:competencias) = (finalizarC (auxCrearRanking (participantesC compe) (categoriaC compe)) dopping compe) : (auxFinalizarCompetencias competencias)

--transcurrirDiaJ :: JJOO -> JJOO
--transcurrirDiaJ juegos = auxTranscurrirDiaJ juegos (jornadaActualJ juegos)
--auxTranscurrirDiaJ :: JJOO -> Int -> JJOO
--auxTranscurrirDiaJ (J _ _ diaActual) _ = (J _ _ auxAumentarDia (diaActual+1))
--auxTranscurrirDiaJ (NuevoDia competencias juegos) 0 = (NuevoDia (auxFinalizarCompetencias competencias) juegos) (-1)
--auxTranscurrirDiaJ (NuevoDia _ juegos) i = auxTranscurrirDiaJ juegos (i-1)

nuevoJConJornadaActual :: Int -> [Atleta] -> [[Competencia]] -> Int -> JJOO
nuevoJConJornadaActual anio atletas [] jornada = (J anio atletas jornada)
nuevoJConJornadaActual anio atletas (compe:competencias) _ = NuevoDia compe (nuevoJ anio atletas competencias)

auxSacarAtletasConPais :: [Atleta] -> Pais -> [Atleta]
auxSacarAtletasConPais [] _ = []
auxSacarAtletasConPais (atle:atletas) pais | (nacionalidadA atle) /= pais = atle : auxSacarAtletasConPais atletas pais
										   | otherwise = auxSacarAtletasConPais atletas pais

auxNuevasCompetenciasSinAtletasConPaisYCat :: [Competencia] -> Pais -> Categoria -> [Competencia]
auxNuevasCompetenciasSinAtletasConPaisYCat [] _ _ = []
auxNuevasCompetenciasSinAtletasConPaisYCat (compe:competencias) pais cat | (categoriaC compe == cat) = (nuevaC (fst(categoriaC compe)) (snd(categoriaC compe)) (auxSacarAtletasConPais (participantesC compe) pais)) : (auxNuevasCompetenciasSinAtletasConPaisYCat competencias pais cat)
																   		 | otherwise = compe : (auxNuevasCompetenciasSinAtletasConPaisYCat competencias pais cat)

auxRecorreCronograma :: JJOO -> (Deporte, Sexo) -> Pais -> [[Competencia]]
auxRecorreCronograma (J _ _ _) _ _ = []
auxRecorreCronograma (NuevoDia competencias juegos) cate pais = (auxRecorreCronograma juegos cate pais) ++ [(auxNuevasCompetenciasSinAtletasConPaisYCat competencias pais cate)]

boicotPorDisciplinaJ :: JJOO -> (Deporte, Sexo) -> Pais -> JJOO
boicotPorDisciplinaJ juegos cat pais = nuevoJConJornadaActual  (anioJ juegos) (atletasJ juegos) (auxRecorreCronograma juegos cat pais) (jornadaActualJ juegos)
