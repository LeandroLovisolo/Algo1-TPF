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

--medalleroJ :: JJOO -> [(Pais, [Integer])]
--medalleroJ j = medallero (paisesQueGanaron j)
--    where medallero [] = []
--          medallero (x:xs) = (x, medalleroPorPais x) : medallero xs
--          paisesQueGanaron j = ?
--          medalleroPorPais p = ?
--          medallistas = ?

medalleroJ :: [Competencia] -> [(Pais, [Int])]
medalleroJ cs = medallero (paisesGanadoresOrdenados cs)
    where medallero [] = []
          medallero (x:xs) = (medalleroPorPais x cs) : medallero xs

paisesGanadoresOrdenados :: [Competencia] -> [Pais]
paisesGanadoresOrdenados cs = ordenar (paisesGanadores cs)
    where ordenar [] = []
          ordenar [p] = [p]
          ordenar (p1:p2:ps) = bubbleSort (p1:p2:ps) [] False
          bubbleSort (p1:p2:ps) acc flag
            | (tieneMasMedallas p1 p2 cs) = bubbleSort (p2:ps) (acc ++ [p1]) flag
            | otherwise                   = bubbleSort (p1:ps) (acc ++ [p2]) True
          bubbleSort [p] acc flag
            | flag == True = bubbleSort (acc ++ [p]) [] False
            | otherwise    = (acc ++ [p])

paisesGanadores :: [Competencia] -> [Pais]
paisesGanadores xs = sinRepetidos (obtenerPaises ((auxMedallistas xs 0) ++
                                                  (auxMedallistas xs 1) ++
                                                  (auxMedallistas xs 2)))
    where obtenerPaises [] = []
          obtenerPaises (x:xs) = (nacionalidadA x) : obtenerPaises xs

-- Devuelve los elementos de una lista sin repetir.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if (elem (last (x:xs)) (init (x:xs)))
                         then sinRepetidos (init (x:xs))
                         else (sinRepetidos (init (x:xs))) ++ [last (x:xs)]

-- Dada una lista de competencias finalizas y una posición, devuelve
-- todos los atletas que finalizaron en esa posición.
auxMedallistas :: [Competencia] -> Int -> [Atleta]
auxMedallistas [] m = []
auxMedallistas (x:xs) m
    | (length (rankingC x)) <= m = auxMedallistas xs m
    | otherwise                  = (rankingC x !! m) : auxMedallistas xs m

tieneMasMedallas :: Pais -> Pais -> [Competencia] -> Bool
tieneMasMedallas p1 p2 cs =
    let m1 = snd (medalleroPorPais p1 cs)
        m2 = snd (medalleroPorPais p2 cs)
    in   (m1 !! 0 >  m2 !! 0) ||
        ((m1 !! 0 == m2 !! 0) && (m1 !! 1 >  m2 !! 1)) ||
        ((m1 !! 0 == m2 !! 0) && (m1 !! 1 == m2 !! 1) && (m1 !! 2 >= m2 !! 2))

medalleroPorPais :: Pais -> [Competencia] -> (Pais, [Int])
medalleroPorPais p [] = (p, [0, 0, 0])
medalleroPorPais p as = (p, (medallero p as))
    where medallero p as = [length (medallas p as 0),
                            length (medallas p as 1),
                            length (medallas p as 2)]
          medallas p as pos = filtrarPorPais p (auxMedallistas as pos)
          filtrarPorPais p [] = []
          filtrarPorPais p (x:xs) = if nacionalidadA x == p
                                    then x : (filtrarPorPais p xs)
                                    else filtrarPorPais p xs

-- Devuelve las competencias finalizadas hasta la jornada actual inclusive. 
competenciasFinalizadas :: JJOO -> [Competencia]
competenciasFinalizadas j = competencias (jornadaActualJ j) j
    where competencias 1 j = soloFinalizadas (cronogramaJ j 1)
          competencias d j = soloFinalizadas (cronogramaJ j d) ++ (competencias (d - 1) j)
          soloFinalizadas [] = []
          soloFinalizadas (c:cs) = if finalizadaC c
                                   then c : soloFinalizadas cs
                                   else soloFinalizadas cs

-- Datos de prueba. Devuelve una lista de competencias finalizadas.
testCompetencias :: [Competencia]
testCompetencias = [(competencia "Futbol"   [111, 222, 333, 555, 444, 777, 888, 666]),
                    (competencia "Handball" [333, 111, 222, 888, 555, 444, 777, 666]),
                    (competencia "Basket"   [111, 555, 333, 444, 888, 222, 666, 777]),
                    (competencia "Volley"   [555, 222, 444, 111, 666, 888, 777, 333])]
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


--Prototipo de transcurrir dia, falta auxCrearRanking y asignar algun al dopping!
--En caso de que la especificacion este mal, porque dice que requiere que no sea el ultimo pero el invariante admite que sea el ultimo dia
auxAumentarDia :: Int -> Int -> Int
auxAumentarDia diaActual maxDias | diaActual < maxDias = diaActual + 1
								 | diaActual == maxDias = diaActual
								 
--Si la lista de atletas inicial no tiene repetidos entonces funciona, si no habri que buscar una forma de sacar los repetidos. 
--El auxOrdenar los ordenar de menor a mayor, para que los ordene de mayor a menor hay que poner un reverse antes de auxOrdenar en el auxCrearRanking
auxCrearRanking :: [Atleta] -> Categoria -> [Int]
auxCrearRanking [] _ = []
auxCrearRanking atletas cate = (ciaNumberA (auxMayorCapacidad atletas cate (head atletas))) : auxCrearRanking (auxSacaUnaVez atletas (auxMayorCapacidad atletas cate (head atletas))) cate

auxMayorCapacidad :: [Atleta] -> Categoria -> Atleta -> Atleta
auxMayorCapacidad [] _ atleMax = atleMax
auxMayorCapacidad (atleta:atletas) cate atleMax | (capacidadA atleta (fst cate)) >= (capacidadA atleMax (fst cate)) = auxMayorCapacidad atletas cate atleta
                        | otherwise = auxMayorCapacidad atletas cate atleMax

auxSacaUnaVez :: [Atleta]-> Atleta -> [Atleta]
auxSacaUnaVez [] at = []
auxSacaUnaVez (atle:atletas) at | (ciaNumberA atle) == (ciaNumberA at) = auxSacaUnaVez atletas at
                | otherwise = atle : (auxSacaUnaVez atletas at)

auxCrearDopping :: Competencia -> [(Int, Bool)]
auxCrearDopping compe | (length (participantesC compe) >=1) = [(ciaNumberA (head (participantesC compe)), True)]
            | otherwise = []

auxFinalizarCompetencias :: [Competencia] -> [Competencia]
auxFinalizarCompetencias [] = []
auxFinalizarCompetencias (compe:competencias) | finalizadaC compe = compe : (auxFinalizarCompetencias competencias)
                        | otherwise =  (finalizarC compe (auxCrearRanking (participantesC compe) (categoriaC compe)) (auxCrearDopping compe)) : (auxFinalizarCompetencias competencias)

transcurrirDiaJ :: JJOO -> JJOO
transcurrirDiaJ juegos = auxTranscurrirDiaJ juegos ((cantDiasJ juegos)-(jornadaActualJ juegos))
auxTranscurrirDiaJ :: JJOO -> Int -> JJOO
-- ver si usar auxAumentarDia
auxTranscurrirDiaJ (J anio atletas diaActual) _ = (J anio atletas (diaActual+1))
auxTranscurrirDiaJ (NuevoDia competencias juegos) 0 = (NuevoDia (auxFinalizarCompetencias competencias) (auxTranscurrirDiaJ juegos (-1)))
auxTranscurrirDiaJ (NuevoDia competencias juegos) i = (NuevoDia competencias (auxTranscurrirDiaJ juegos (i-1)))

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
