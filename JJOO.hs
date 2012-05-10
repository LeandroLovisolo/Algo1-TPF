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
    | auxExisteAtletaConCia atletasParaSacar (ciaNumberA atle) =
        auxSacarAtletas atletas atletasParaSacar
    | otherwise = atle : auxSacarAtletas atletas atletasParaSacar

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
auxTranscurrirDiaJ (NuevoDia competencias juegos) 0 =
    (NuevoDia (auxFinalizarCompetencias competencias) (auxTranscurrirDiaJ juegos (-1)))
auxTranscurrirDiaJ (NuevoDia competencias juegos) i =
    (NuevoDia competencias (auxTranscurrirDiaJ juegos (i-1)))

auxFinalizarCompetencias :: [Competencia] -> [Competencia]
auxFinalizarCompetencias [] = []
auxFinalizarCompetencias (compe:competencias)
    | finalizadaC compe = compe : (auxFinalizarCompetencias competencias)
    | otherwise         = (finalizarC compe
                                      (auxCrearRanking (participantesC compe) (categoriaC compe))
                                      (auxCrearDopping compe))
                                : (auxFinalizarCompetencias competencias)

auxCrearRanking :: [Atleta] -> Categoria -> [Int]
auxCrearRanking [] _ = []
auxCrearRanking atletas cate =
    (ciaNumberA (auxMayorCapacidad atletas cate (head atletas))) :
        (auxCrearRanking (auxSacaUnaVez atletas (auxMayorCapacidad
                                                 atletas cate (head atletas))) cate)

auxSacaUnaVez :: [Atleta]-> Atleta -> [Atleta]
auxSacaUnaVez [] at = []
auxSacaUnaVez (atle:atletas) at
    | (ciaNumberA atle) == (ciaNumberA at) = auxSacaUnaVez atletas at
    | otherwise                            = atle : (auxSacaUnaVez atletas at)

auxMayorCapacidad :: [Atleta] -> Categoria -> Atleta -> Atleta
auxMayorCapacidad [] _ atleMax = atleMax
auxMayorCapacidad (atleta:atletas) cate atleMax
    | (capacidadA atleta (fst cate)) >= (capacidadA atleMax (fst cate)) = 
        auxMayorCapacidad atletas cate atleta
    | otherwise = auxMayorCapacidad atletas cate atleMax

auxCrearDopping :: Competencia -> [(Int, Bool)]
auxCrearDopping compe
    | (length (participantesC compe) >=1) = [(ciaNumberA (head (participantesC compe)), True)]
    | otherwise                           = []

auxAumentarDia :: Int -> Int -> Int
auxAumentarDia diaActual maxDias
    | diaActual < maxDias = diaActual + 1
	| diaActual == maxDias = diaActual
								 
-------------------------------------------------------------------------------
-- Fin de transcurrirDiaJ -----------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- boicotPorDisciplinaJ -------------------------------------------------------
-------------------------------------------------------------------------------

boicotPorDisciplinaJ :: JJOO -> (Deporte, Sexo) -> Pais -> JJOO
boicotPorDisciplinaJ (J anio atletas jornadaActual) _ _ = (J anio atletas jornadaActual)
boicotPorDisciplinaJ (NuevoDia competencias juegos) cate pais = 
    (NuevoDia (auxCompetenciasSinAtletasConPaisYCat competencias pais cate) (boicotPorDisciplinaJ juegos cate pais))


auxAtletasConCia :: [Int] -> [Atleta] -> [Atleta]
auxAtletasConCia _ [] = []
auxAtletasConCia ciaNumbers (atle:atletas)
  | elem (ciaNumberA atle) ciaNumbers = atle : (auxAtletasConCia ciaNumbers atletas)
  | otherwise = auxAtletasConCia ciaNumbers atletas

auxRankingSinAtletasConPais :: [Atleta] -> Pais -> [Int]
auxRankingSinAtletasConPais (atle:atletas) pais | (nacionalidadA atle) == pais = auxRankingSinAtletasConPais atletas pais
                                                | otherwise = (ciaNumberA atle) : (auxRankingSinAtletasConPais atletas pais)

auxDoppingSinAtletasConPais :: [Atleta] -> Pais -> Competencia -> [(Int, Bool)]
auxDoppingSinAtletasConPais (atle:atletas) pais compe | (nacionalidadA atle) == pais = auxDoppingSinAtletasConPais atletas pais compe
                                          | otherwise = (ciaNumberA atle,leDioPositivoC compe atle) : (auxDoppingSinAtletasConPais atletas pais compe)

auxCompetenciasSinAtletasConPaisYCat :: [Competencia] -> Pais -> Categoria -> [Competencia]
auxCompetenciasSinAtletasConPaisYCat [] _ _ = []
auxCompetenciasSinAtletasConPaisYCat (compe:competencias) pais cat
    | (categoriaC compe == cat) && (not (finalizadaC compe)) =
        (nuevaC (fst(categoriaC compe))
                (snd(categoriaC compe))
                (auxSacarAtletasConPais (participantesC compe) pais))
            : (auxCompetenciasSinAtletasConPaisYCat competencias pais cat)
    | (categoriaC compe == cat) && (finalizadaC compe) = 
                (finalizarC (nuevaC (fst(categoriaC compe)) (snd(categoriaC compe)) 
                  (auxSacarAtletasConPais (participantesC compe) pais))
                (auxRankingSinAtletasConPais (rankingC compe) pais) 
                (auxDoppingSinAtletasConPais (lesTocoControlAntiDopingC compe) pais compe)
                )
            : (auxCompetenciasSinAtletasConPaisYCat competencias pais cat)
    | otherwise = compe : (auxCompetenciasSinAtletasConPaisYCat competencias pais cat)



auxSacarAtletasConPais :: [Atleta] -> Pais -> [Atleta]
auxSacarAtletasConPais [] _ = []
auxSacarAtletasConPais (atle:atletas) pais
    | (nacionalidadA atle) /= pais = atle : auxSacarAtletasConPais atletas pais
    | otherwise                    = auxSacarAtletasConPais atletas pais

-------------------------------------------------------------------------------
-- Fin de boicotPorDisciplinaJ ------------------------------------------------
-------------------------------------------------------------------------------


losMasFracasadosJ         = undefined
liuSongJ                  = undefined
stevenBradburyJ           = undefined
uyOrdenadoAsiHayUnPatronJ = undefined
sequiaOlimpicaJ           = undefined
