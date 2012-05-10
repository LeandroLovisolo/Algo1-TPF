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

nuevoJ :: Int -> [Atleta] -> [[Competencia]] -> JJOO
nuevoJ anio atletas [] = (J anio atletas 1)
nuevoJ anio atletas (x:xs) = NuevoDia x (nuevoJ anio atletas xs)

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
cronogramaJ (J _ _ _) _ = []
cronogramaJ (NuevoDia competencias juegos) dias 
    | dias == (cantDiasJ juegos) + 1 = competencias
    | otherwise                      = cronogramaJ juegos dias

jornadaActualJ :: JJOO -> Int
jornadaActualJ (J _ _ jornadaActual) = jornadaActual
jornadaActualJ (NuevoDia _ juegos) = jornadaActualJ juegos


-------------------------------------------------------------------------------
-- dePaseoJ -------------------------------------------------------------------
-------------------------------------------------------------------------------

dePaseoJ :: JJOO -> [Atleta]
dePaseoJ juegos = auxDePaseoJ juegos (atletasJ juegos)

auxDePaseoJ :: JJOO -> [Atleta] -> [Atleta]
auxDePaseoJ (J _ _ _) atles = atles
auxDePaseoJ (NuevoDia (compe:competencias) juegos) atles =
    auxDePaseoJ (NuevoDia competencias juegos) (auxSacarAtletas atles (participantesC compe))
auxDePaseoJ (NuevoDia [] juegos) atles = auxDePaseoJ juegos atles

auxSacarAtletas :: [Atleta] -> [Atleta] -> [Atleta]
auxSacarAtletas atletas [] = []
auxSacarAtletas [] atletasParaSacar = []
auxSacarAtletas (atle:atletas) atletasParaSacar
    | auxExisteAtletaConCia atletasParaSacar (ciaNumberA atle) = auxSacarAtletas atletas atletasParaSacar
    | otherwise                                                = atle : auxSacarAtletas atletas atletasParaSacar

auxExisteAtletaConCia :: [Atleta] -> Int -> Bool
auxExisteAtletaConCia [] _ = False
auxExisteAtletaConCia (atle:atletas) cia
    | ((ciaNumberA atle) == cia) = True
    | otherwise = auxExisteAtletaConCia atletas cia

-------------------------------------------------------------------------------
-- Fin de dePaseoJ ------------------------------------------------------------
-------------------------------------------------------------------------------    


-------------------------------------------------------------------------------
-- medalleroJ -----------------------------------------------------------------
-------------------------------------------------------------------------------

medalleroJ :: JJOO -> [(Pais, [Int])]
medalleroJ j = medallero (paisesGanadoresOrdenados j)
    where medallero [] = []
          medallero (x:xs) = (medalleroPorPais x j) : medallero xs

paisesGanadoresOrdenados :: JJOO -> [Pais]
paisesGanadoresOrdenados j = ordenar (paisesGanadores j)
    where ordenar [] = []
          ordenar [p] = [p]
          ordenar (p1:p2:ps) = bubbleSort (p1:p2:ps) [] False
          bubbleSort (p1:p2:ps) acc flag
            | (tieneMasMedallas p1 p2 j) = bubbleSort (p2:ps) (acc ++ [p1]) flag
            | otherwise                  = bubbleSort (p1:ps) (acc ++ [p2]) True
          bubbleSort [p] acc flag
            | flag == True = bubbleSort (acc ++ [p]) [] False
            | otherwise    = (acc ++ [p])

paisesGanadores :: JJOO -> [Pais]
paisesGanadores j = sinRepetidos (obtenerPaises ((medallistas 0 j) ++
                                                 (medallistas 1 j) ++
                                                 (medallistas 2 j)))
    where obtenerPaises [] = []
          obtenerPaises (x:xs) = (nacionalidadA x) : obtenerPaises xs

-- Devuelve los elementos de una lista sin repetir.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if (elem (last (x:xs)) (init (x:xs)))
                         then sinRepetidos (init (x:xs))
                         else (sinRepetidos (init (x:xs))) ++ [last (x:xs)]

-- Dados unos JJOO y una posición, devuelve todos los atletas
-- que finalizaron alguna competencia en esa posición.
medallistas :: Int -> JJOO -> [Atleta]
medallistas m j = obtenerMedallistas (competenciasFinalizadas j) m
    where obtenerMedallistas [] m = []
          obtenerMedallistas (x:xs) m
              | (length (rankingC x)) <= m = obtenerMedallistas xs m
              | otherwise                  = (rankingC x !! m) : obtenerMedallistas xs m

-- Devuelve las competencias finalizadas hasta la jornada actual inclusive. 
competenciasFinalizadas :: JJOO -> [Competencia]
competenciasFinalizadas j = competencias (jornadaActualJ j) j
    where competencias 1 j = soloFinalizadas (cronogramaJ j 1)
          competencias d j = soloFinalizadas (cronogramaJ j d) ++ (competencias (d - 1) j)
          soloFinalizadas [] = []
          soloFinalizadas (c:cs) = if finalizadaC c
                                   then c : soloFinalizadas cs
                                   else soloFinalizadas cs

tieneMasMedallas :: Pais -> Pais -> JJOO -> Bool
tieneMasMedallas p1 p2 j =
    let m1 = snd (medalleroPorPais p1 j)
        m2 = snd (medalleroPorPais p2 j)
    in   (m1 !! 0 >  m2 !! 0) ||
        ((m1 !! 0 == m2 !! 0) && (m1 !! 1 >  m2 !! 1)) ||
        ((m1 !! 0 == m2 !! 0) && (m1 !! 1 == m2 !! 1) && (m1 !! 2 >= m2 !! 2))

medalleroPorPais :: Pais -> JJOO -> (Pais, [Int])
medalleroPorPais p j = (p, (medallero p j))
    where medallero p j = [length (medallas p 0 j),
                           length (medallas p 1 j),
                           length (medallas p 2 j)]
          medallas p m j = filtrarPorPais p (medallistas m j)
          filtrarPorPais p [] = []
          filtrarPorPais p (x:xs) = if nacionalidadA x == p
                                    then x : (filtrarPorPais p xs)
                                    else filtrarPorPais p xs

-------------------------------------------------------------------------------
-- Fin de medalleroJ ----------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- transcurrirDiaJ ------------------------------------------------------------
-------------------------------------------------------------------------------

transcurrirDiaJ :: JJOO -> JJOO
transcurrirDiaJ juegos = auxTranscurrirDiaJ juegos ((cantDiasJ juegos)-(jornadaActualJ juegos))

auxTranscurrirDiaJ :: JJOO -> Int -> JJOO
-- ver si usar auxAumentarDia
auxTranscurrirDiaJ (J anio atletas diaActual) _ = (J anio atletas (diaActual+1))
auxTranscurrirDiaJ (NuevoDia competencias juegos) 0 = (NuevoDia (auxFinalizarCompetencias competencias) (auxTranscurrirDiaJ juegos (-1)))
auxTranscurrirDiaJ (NuevoDia competencias juegos) i = (NuevoDia competencias (auxTranscurrirDiaJ juegos (i-1)))

auxFinalizarCompetencias :: [Competencia] -> [Competencia]
auxFinalizarCompetencias [] = []
auxFinalizarCompetencias (compe:competencias) | finalizadaC compe = compe : (auxFinalizarCompetencias competencias)
                        | otherwise =  (finalizarC compe (auxCrearRanking (participantesC compe) (categoriaC compe)) (auxCrearDopping compe)) : (auxFinalizarCompetencias competencias)

--Si la lista de atletas inicial no tiene repetidos entonces funciona, si no habri que buscar una forma de sacar los repetidos. 
--El auxOrdenar los ordenar de menor a mayor, para que los ordene de mayor a menor hay que poner un reverse antes de auxOrdenar en el auxCrearRanking
auxCrearRanking :: [Atleta] -> Categoria -> [Int]
auxCrearRanking [] _ = []
auxCrearRanking atletas cate = (ciaNumberA (auxMayorCapacidad atletas cate (head atletas))) : auxCrearRanking (auxSacaUnaVez atletas (auxMayorCapacidad atletas cate (head atletas))) cate

auxSacaUnaVez :: [Atleta]-> Atleta -> [Atleta]
auxSacaUnaVez [] at = []
auxSacaUnaVez (atle:atletas) at | (ciaNumberA atle) == (ciaNumberA at) = auxSacaUnaVez atletas at
                | otherwise = atle : (auxSacaUnaVez atletas at)

auxMayorCapacidad :: [Atleta] -> Categoria -> Atleta -> Atleta
auxMayorCapacidad [] _ atleMax = atleMax
auxMayorCapacidad (atleta:atletas) cate atleMax | (capacidadA atleta (fst cate)) >= (capacidadA atleMax (fst cate)) = auxMayorCapacidad atletas cate atleta
                        | otherwise = auxMayorCapacidad atletas cate atleMax


auxCrearDopping :: Competencia -> [(Int, Bool)]
auxCrearDopping compe | (length (participantesC compe) >=1) = [(ciaNumberA (head (participantesC compe)), True)]
            | otherwise = []


--Prototipo de transcurrir dia, falta auxCrearRanking y asignar algun al dopping!
--En caso de que la especificacion este mal, porque dice que requiere que no sea el ultimo pero el invariante admite que sea el ultimo dia
auxAumentarDia :: Int -> Int -> Int
auxAumentarDia diaActual maxDias | diaActual < maxDias = diaActual + 1
								 | diaActual == maxDias = diaActual
								 
-------------------------------------------------------------------------------
-- Fin de transcurrirDiaJ -----------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- boicotPorDisciplinaJ -------------------------------------------------------
-------------------------------------------------------------------------------

boicotPorDisciplinaJ :: JJOO -> (Deporte, Sexo) -> Pais -> JJOO
boicotPorDisciplinaJ juegos cat pais = nuevoJConJornadaActual  (anioJ juegos) (atletasJ juegos) (auxRecorreCronograma juegos cat pais) (jornadaActualJ juegos)

nuevoJConJornadaActual :: Int -> [Atleta] -> [[Competencia]] -> Int -> JJOO
nuevoJConJornadaActual anio atletas [] jornada = (J anio atletas jornada)
nuevoJConJornadaActual anio atletas (compe:competencias) _ = NuevoDia compe (nuevoJ anio atletas competencias)

auxRecorreCronograma :: JJOO -> (Deporte, Sexo) -> Pais -> [[Competencia]]
auxRecorreCronograma (J _ _ _) _ _ = []
auxRecorreCronograma (NuevoDia competencias juegos) cate pais = (auxRecorreCronograma juegos cate pais) ++ [(auxNuevasCompetenciasSinAtletasConPaisYCat competencias pais cate)]

auxNuevasCompetenciasSinAtletasConPaisYCat :: [Competencia] -> Pais -> Categoria -> [Competencia]
auxNuevasCompetenciasSinAtletasConPaisYCat [] _ _ = []
auxNuevasCompetenciasSinAtletasConPaisYCat (compe:competencias) pais cat | (categoriaC compe == cat) = (nuevaC (fst(categoriaC compe)) (snd(categoriaC compe)) (auxSacarAtletasConPais (participantesC compe) pais)) : (auxNuevasCompetenciasSinAtletasConPaisYCat competencias pais cat)
                                                                         | otherwise = compe : (auxNuevasCompetenciasSinAtletasConPaisYCat competencias pais cat)


auxSacarAtletasConPais :: [Atleta] -> Pais -> [Atleta]
auxSacarAtletasConPais [] _ = []
auxSacarAtletasConPais (atle:atletas) pais | (nacionalidadA atle) /= pais = atle : auxSacarAtletasConPais atletas pais
                                           | otherwise = auxSacarAtletasConPais atletas pais

-------------------------------------------------------------------------------
-- Fin de boicotPorDisciplinaJ ------------------------------------------------
-------------------------------------------------------------------------------


losMasFracasadosJ         = undefined
liuSongJ                  = undefined
stevenBradburyJ           = undefined
uyOrdenadoAsiHayUnPatronJ = undefined
sequiaOlimpicaJ           = undefined


-------------------------------------------------------------------------------
-- Datos de prueba ------------------------------------------------------------
-------------------------------------------------------------------------------

testJJOODePaseo :: JJOO
testJJOODePaseo = NuevoDia dia2 (NuevoDia dia1 (J 2012 (atletasActivos ++ atletasDePaseo) 1))
    where atletasActivos = [(nuevoA "Abel"     Masculino 18 "Argentina" 111),
                            (nuevoA "Beto"     Masculino 19 "Brasil"    222),
                            (nuevoA "Carlos"   Masculino 20 "Chile"     333),
                            (nuevoA "Daniel"   Masculino 21 "Dinamarca" 444)]
          atletasDePaseo = [(nuevoA "Esteban"  Masculino 22 "Ecuador"   555),
                            (nuevoA "Federico" Masculino 22 "Francia"   666)]
          dia1 = [(competencia "Futbol"),
                  (competencia "Handball"),
                  (competencia "Basket")]
          dia2 = [(competencia "Volley"),
                  (competencia "Arqueria"),
                  (competencia "Natacion")]
          competencia d = (nuevaC d Masculino atletasActivos)

-- Devuelve unos JJOO con algunas competencias finalizadas y otras no.
testJJOO :: JJOO
testJJOO = NuevoDia dia3 (NuevoDia dia2 (NuevoDia dia1 (J 2012 testAtletas 2)))
    where cronograma = [dia1, dia2, dia3]
          dia1 = [(competenciaF "Futbol"   [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competenciaF "Handball" [333, 111, 222, 888, 555, 444, 777, 666]),
                  (competenciaF "Basket"   [111, 555, 333, 444, 888, 222, 666, 777])]
          dia2 = [(competenciaF "Volley"   [555, 222, 444, 111, 666, 888, 777, 333]),
                  (competencia  "Arqueria"),
                  (competencia  "Natacion")]
          dia3 = [(competencia  "Gimnasia Artistica"),
                  (competencia  "Hockey"),
                  (competencia  "Rugby")]
          competenciaF dep pos = finalizarC (nuevaC dep Masculino testAtletas) pos []
          competencia  dep     = (nuevaC dep Masculino testAtletas)

-- Devuelve una lista de competencias finalizadas.
testCompetencias :: [Competencia]
testCompetencias = [(competencia "Futbol"   [111, 222, 333, 555, 444, 777, 888, 666]),
                    (competencia "Handball" [333, 111, 222, 888, 555, 444, 777, 666]),
                    (competencia "Basket"   [111, 555, 333, 444, 888, 222, 666, 777]),
                    (competencia "Volley"   [555, 222, 444, 111, 666, 888, 777, 333])]
    where competencia dep pos = finalizarC (nuevaC dep Masculino testAtletas) pos []

-- Devuelve una lista de atletas entrenados en ciertos deportes.
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
        deportes = ["Futbol", "Handball", "Basket", "Volley"]
        entrenarDeportes atleta = foldl entrenarDeporte atleta deportes
        entrenarDeporte atleta deporte = entrenarDeporteA atleta deporte 10
