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

nuevoJ :: Int -> [Atleta] -> [[Competencia]] -> JJOO
nuevoJ anio atletas [] = (J anio atletas 1)
nuevoJ anio atletas xs = NuevoDia (last xs) (nuevoJ anio atletas (init xs))

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
dePaseoJ (J _ atletas _) = atletas
dePaseoJ (NuevoDia [] juegos) = dePaseoJ juegos
dePaseoJ (NuevoDia (compe:competencias) juegos) = dePaseoJ (juegoSinAtletas (NuevoDia competencias juegos) (participantesC compe)) 

juegoSinAtletas :: JJOO -> [Atleta] -> JJOO
juegoSinAtletas (J anio atletas jornada) atletasARemover = (J anio (removerAtletas atletas atletasARemover) jornada)
juegoSinAtletas (NuevoDia competencias juegos) atletasARemover = (NuevoDia competencias (juegoSinAtletas juegos atletasARemover))

removerAtletas :: [Atleta] -> [Atleta] -> [Atleta]
removerAtletas atletas [] = atletas 
removerAtletas [] _ = []
removerAtletas (atle:atletas) atletasARemover | elem (ciaNumberA atle) (auxAtletasACias atletasARemover) = 
                                                removerAtletas atletas atletasARemover
                                              | otherwise = atle : (removerAtletas atletas atletasARemover)
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
         (m1 !! 0 >  m2 !! 0) ||
        ((m1 !! 0 == m2 !! 0) && (m1 !! 1 >  m2 !! 1)) ||
        ((m1 !! 0 == m2 !! 0) && (m1 !! 1 == m2 !! 1) && (m1 !! 2 >= m2 !! 2))
    where m1 = snd (medalleroPorPais p1 j)
          m2 = snd (medalleroPorPais p2 j)

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
transcurrirDiaJ j = transcurrir j ((cantDiasJ j) - (jornadaActualJ j))
    where transcurrir (J anio atletas diaActual) _ = (J anio atletas (diaActual + 1))
          transcurrir (NuevoDia cs j) 0 = (NuevoDia (auxFinalizarCompetencias cs) (transcurrir j (-1)))
          transcurrir (NuevoDia cs j) i = (NuevoDia cs (transcurrir j (i - 1)))

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
auxCrearDopping c
    | length (participantesC c) >= 1 = [(ciaNumberA (head (participantesC c)), False)]
    | otherwise                        = []

-------------------------------------------------------------------------------
-- Fin de transcurrirDiaJ -----------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- boicotPorDisciplinaJ -------------------------------------------------------
-------------------------------------------------------------------------------

boicotPorDisciplinaJ :: JJOO -> (Deporte, Sexo) -> Pais -> JJOO
boicotPorDisciplinaJ (J anio atletas jornadaActual) _ _ = (J anio atletas jornadaActual)
boicotPorDisciplinaJ (NuevoDia competencias juegos) cate pais = 
    (NuevoDia (auxCompetenciasSinAtletasConPaisYCat competencias pais cate)
              (boicotPorDisciplinaJ juegos cate pais))

auxRankingSinAtletasConPais :: [Atleta] -> Pais -> [Int]
auxRankingSinAtletasConPais [] _ = []
auxRankingSinAtletasConPais (atle:atletas) pais
    | (nacionalidadA atle) == pais = auxRankingSinAtletasConPais atletas pais
    | otherwise                    = (ciaNumberA atle) : (auxRankingSinAtletasConPais atletas pais)

auxDoppingSinAtletasConPais :: [Atleta] -> Pais -> Competencia -> [(Int, Bool)]
auxDoppingSinAtletasConPais [] _ _ = []
auxDoppingSinAtletasConPais (atle:atletas) pais compe
    | (nacionalidadA atle) == pais = auxDoppingSinAtletasConPais atletas pais compe
    | otherwise                    = (ciaNumberA atle, leDioPositivoC compe atle) :
                                     (auxDoppingSinAtletasConPais atletas pais compe)

auxCompetenciasSinAtletasConPaisYCat :: [Competencia] -> Pais -> Categoria -> [Competencia]
auxCompetenciasSinAtletasConPaisYCat [] _ _ = []
auxCompetenciasSinAtletasConPaisYCat (compe:competencias) pais cat
    | (categoriaC compe == cat) && (not (finalizadaC compe)) =
        (nuevaC (fst(categoriaC compe))
                (snd(categoriaC compe))
                (auxSacarAtletasConPais (participantesC compe) pais)) :
        (auxCompetenciasSinAtletasConPaisYCat competencias pais cat)
    | (categoriaC compe == cat) && (finalizadaC compe) = 
                (finalizarC (nuevaC (fst(categoriaC compe))
                                    (snd(categoriaC compe)) 
                            (auxSacarAtletasConPais (participantesC compe) pais))
                            (auxRankingSinAtletasConPais (rankingC compe) pais) 
                            (auxDoppingSinAtletasConPais (lesTocoControlAntiDopingC compe)
                                                         pais compe)) :
                (auxCompetenciasSinAtletasConPaisYCat competencias pais cat)
    | otherwise = compe : (auxCompetenciasSinAtletasConPaisYCat competencias pais cat)

auxSacarAtletasConPais :: [Atleta] -> Pais -> [Atleta]
auxSacarAtletasConPais [] _ = []
auxSacarAtletasConPais (atle:atletas) pais
    | (nacionalidadA atle) /= pais = atle : auxSacarAtletasConPais atletas pais
    | otherwise                    = auxSacarAtletasConPais atletas pais

-------------------------------------------------------------------------------
-- Fin de boicotPorDisciplinaJ ------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- losMasFracasadosJ-----------------------------------------------------------
-------------------------------------------------------------------------------

losMasFracasadosJ :: JJOO->Pais->[Atleta]
losMasFracasadosJ (J _ atletas _) p = atletas
losMasFracasadosJ (NuevoDia competencias juegos) pais =
    auxPerdedores (NuevoDia competencias juegos)
                  (auxordenAtletas (NuevoDia competencias juegos)
                                   (auxAtletasPais (atletasJ juegos) pais))

auxPerdedores :: JJOO->[Atleta]->[Atleta]
auxPerdedores juegos [] = []
auxPerdedores juegos (a:as)
    | not(ganoMedalla (competenciasFinalizadas juegos) a) = a : auxPerdedores juegos as
    | otherwise = auxPerdedores juegos as
        where ganoMedalla c a = (ganoOro c a || ganoPlata c a || ganoBronce c a)
              ganoOro [] a = False
              ganoOro (comp:competencia) a
                | ciaNumberA ((rankingC comp) !! 0) == ciaNumberA a = True
                | otherwise                                         = ganoOro competencia a
              ganoPlata [] a = False
              ganoPlata (comp:competencia) a
                | ciaNumberA ((rankingC comp) !! 1) == ciaNumberA a = True
                | otherwise                                         = ganoPlata competencia a
              ganoBronce [] a = False
              ganoBronce (comp:competencia) a
                | ciaNumberA ((rankingC comp)!!2) == ciaNumberA a = True
                | otherwise                                       = ganoBronce competencia a

auxordenAtletas :: JJOO->[Atleta]-> [Atleta]
auxordenAtletas juegos [] = []
auxordenAtletas juegos [x] = [x]
auxordenAtletas juegos (x:y:xs)
    | cantCompetencias juegos x  >= cantCompetencias juegos y = x : auxordenAtletas juegos (y:xs)
    | cantCompetencias juegos x < cantCompetencias juegos y   = y : auxordenAtletas juegos (x:xs)

cantCompetencias :: JJOO->Atleta->Int
cantCompetencias (J _ _ _) _ = 0
cantCompetencias juegos atleta = auxCompe (competenciasFinalizadas juegos) atleta

auxCompe :: [Competencia] -> Atleta -> Int
auxCompe [] _ = 0
auxCompe (compe:competencias) atleta
    | auxPertenece (participantesC compe) atleta = 1 + auxCompe competencias atleta
    | otherwise                                  = auxCompe competencias atleta

auxPertenece :: [Atleta]->Atleta->Bool
auxPertenece [] a = False
auxPertenece (atleta:atletas) a
    | ciaNumberA a == ciaNumberA atleta = True
    | otherwise                         = auxPertenece atletas a  

auxAtletasPais :: [Atleta]->Pais->[Atleta]
auxAtletasPais [] _ = []
auxAtletasPais (x:xs) p
    | nacionalidadA x == p = x : auxAtletasPais xs p
    | otherwise            = auxAtletasPais xs p

-------------------------------------------------------------------------------
-- Fin de losMasFracasadosJ ---------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- liuSongJ -------------------------------------------------------------------
-------------------------------------------------------------------------------

liuSongJ :: JJOO -> Atleta -> Pais -> JJOO
liuSongJ (J anio atletas jornadaActual) atletaACambiar pais =
    (J anio (auxCambiaNacionalidadAtleta atletas atletaACambiar pais) jornadaActual)
liuSongJ (NuevoDia competencias juegos) atletaACambiar pais = 
    (NuevoDia (auxProcesaCompetencias competencias atletaACambiar pais)
              (liuSongJ juegos atletaACambiar pais))

auxEntrenarDeportes :: Atleta -> Atleta -> [Deporte] -> Atleta
auxEntrenarDeportes atleta atletaOriginal [] = atleta
auxEntrenarDeportes atleta atletaOriginal (deporte:deportes) = 
  auxEntrenarDeportes (entrenarDeporteA atleta deporte (capacidadA atletaOriginal deporte))
                      atletaOriginal deportes

auxCambiaNacionalidadAtleta :: [Atleta] -> Atleta -> Pais -> [Atleta]
auxCambiaNacionalidadAtleta [] _ _ = []
auxCambiaNacionalidadAtleta (atle:atletas) atleACambiar pais
    | (ciaNumberA atle) == (ciaNumberA atleACambiar) =
        (auxEntrenarDeportes (nuevoA (nombreA atle) (sexoA atle) (anioNacimientoA atle)
                                     pais (ciaNumberA atle))
                             atle (deportesA atle)) :
        (auxCambiaNacionalidadAtleta atletas atleACambiar pais)
    | otherwise = atle : (auxCambiaNacionalidadAtleta atletas atleACambiar pais)

auxAtletasACias :: [Atleta] -> [Int]
auxAtletasACias [] = []
auxAtletasACias (atle:atletas) = (ciaNumberA atle) : (auxAtletasACias atletas)

auxRecrearDopping :: Competencia -> [Atleta] -> [(Int,Bool)]
auxRecrearDopping _ [] = []
auxRecrearDopping compe (atle:atletas) =
    (ciaNumberA atle, leDioPositivoC compe atle) : (auxRecrearDopping compe atletas)

auxCambiaNacionalidadAtletaEnCompetencia :: Competencia -> Atleta -> Pais -> Competencia
auxCambiaNacionalidadAtletaEnCompetencia compe atletaACambiar pais
    | finalizadaC compe = finalizarC (nuevaC (fst (categoriaC compe))
                                             (snd (categoriaC compe))
                                             (auxCambiaNacionalidadAtleta (participantesC compe)
                                                                          atletaACambiar pais))
                                     (auxAtletasACias (rankingC compe))
                                     (auxRecrearDopping compe (lesTocoControlAntiDopingC compe))
    | otherwise = nuevaC (fst (categoriaC compe))
                         (snd (categoriaC compe)) 
                         (auxCambiaNacionalidadAtleta (participantesC compe) atletaACambiar pais)

auxProcesaCompetencias :: [Competencia] -> Atleta -> Pais -> [Competencia]
auxProcesaCompetencias [] _ _ = []
auxProcesaCompetencias (compe:competencias) atletaACambiar pais = 
    (auxCambiaNacionalidadAtletaEnCompetencia compe atletaACambiar pais) :
    (auxProcesaCompetencias competencias atletaACambiar pais)

-------------------------------------------------------------------------------
-- Fin de liuSongJ ------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- uyOrdenadoAsiHayUnPatronJ --------------------------------------------------
-------------------------------------------------------------------------------

uyOrdenadoAsiHayUnPatronJ :: JJOO -> Bool
uyOrdenadoAsiHayUnPatronJ juegos = auxUyOrdenadoAsiHayUnPatronJ juegos []

auxUyOrdenadoAsiHayUnPatronJ :: JJOO -> [Pais] -> Bool
auxUyOrdenadoAsiHayUnPatronJ (J _ _ _) lista = (auxExistePatron lista ((length lista) - 1)
                                                                      ((length lista) - 1))
auxUyOrdenadoAsiHayUnPatronJ (NuevoDia competencias juegos) paises
    | auxExisteAlgunoConRanking competencias =
        auxUyOrdenadoAsiHayUnPatronJ juegos
                                     ((auxMejorPaisEnElDia (auxPaisesGanadoresEnElDia competencias [])
                                      (head(auxPaisesGanadoresEnElDia competencias []))):paises)
    | otherwise = auxUyOrdenadoAsiHayUnPatronJ juegos paises

auxExisteAlgunoConRanking :: [Competencia] -> Bool
auxExisteAlgunoConRanking [] = False
auxExisteAlgunoConRanking (compe:competencias)
    | (finalizadaC compe) = ((length (rankingC compe)) > 0)
    | otherwise           = auxExisteAlgunoConRanking competencias

auxMejorPaisEnElDia :: [(Pais, Int)] -> (Pais, Int) -> Pais
auxMejorPaisEnElDia [] pais = (fst pais)
auxMejorPaisEnElDia (pais:paises) paisMax 
    |  ((snd paisMax) > (snd pais)) ||
      (((snd paisMax) > (snd pais)) &&
       ((fst paisMax) > (fst pais))) = auxMejorPaisEnElDia paises paisMax
    | otherwise                      = auxMejorPaisEnElDia paises pais

auxMeterPais :: [(Pais, Int)] -> Pais -> [(Pais, Int)]
auxMeterPais [] pais = [(pais,1)]
auxMeterPais (pais:paises) paisAMeter
    | ((fst pais) == paisAMeter) = (fst pais, (snd(pais)+1)) : paises
    | otherwise                  = pais : (auxMeterPais paises paisAMeter)

auxPaisesGanadoresEnElDia :: [Competencia] -> [(Pais, Int)] -> [(Pais, Int)]
auxPaisesGanadoresEnElDia [] paises = paises
auxPaisesGanadoresEnElDia (compe:competencias) paises
    | (finalizadaC compe) && (length (rankingC compe) > 0) =
            auxPaisesGanadoresEnElDia competencias
                                      (auxMeterPais paises (nacionalidadA ((rankingC compe) !! 0)))
    | otherwise = auxPaisesGanadoresEnElDia competencias paises

auxRecorreYCompara :: [Pais] -> Pais -> Pais -> Int -> Int -> Bool
auxRecorreYCompara paises paisBuscado paisSiguiente 0 maximo
    | ((paises!!0) == paisBuscado) && ((paises!!1) == paisSiguiente) = True
    | otherwise                                                      = True

auxRecorreYCompara paises paisBuscado paisSiguiente indice maximo
    |  (indice < maximo) &&
      ((paises!!indice) == paisBuscado) = ((paises!!(indice+1)) == paisSiguiente) &&
                                           (auxRecorreYCompara paises paisBuscado
                                                               paisSiguiente (indice-1) maximo)
    |  (indice == maximo) &&
      ((paises!!indice) == paisBuscado) = True && (auxRecorreYCompara paises paisBuscado
                                                                      paisSiguiente (indice-1) maximo)
    | otherwise = (auxRecorreYCompara paises paisBuscado paisSiguiente (indice-1) maximo)

auxExistePatron :: [Pais] -> Int -> Int -> Bool
auxExistePatron [] _ _ = True
auxExistePatron [x] _ _ = True
auxExistePatron [x,y] _ _ = True
auxExistePatron paises 0 maximo = auxRecorreYCompara paises (paises!!0) (paises!!1) maximo maximo
auxExistePatron paises indice maximo
    | (indice == maximo) = True && (auxExistePatron paises (indice-1) maximo)
    | otherwise          = (auxRecorreYCompara paises (paises !! indice)
                                              (paises !! (indice + 1))
                                               maximo maximo) && 
                           (auxExistePatron paises (indice-1) maximo)

-------------------------------------------------------------------------------
-- Fin de uyOrdenadoAsiHayUnPatronJ -------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- stevenBradburyJ ------------------------------------------------------------
-------------------------------------------------------------------------------

stevenBradburyJ :: JJOO -> Atleta
stevenBradburyJ j = buscarElMenosCapaz (tuplasMedallistasCapacidad j)
    where buscarElMenosCapaz [x]       = fst x
          buscarElMenosCapaz (x:xs)
            | esElMenosCapaz x (x:xs)  = fst x
            | otherwise                = buscarElMenosCapaz xs
          esElMenosCapaz _ []          = True 
          esElMenosCapaz a (x:xs)      = (snd(a) <= snd(x)) && (esElMenosCapaz a xs)
          tuplasMedallistasCapacidad j = obtenerTuplas (competenciasFinalizadas j)
          obtenerTuplas []             = []
          obtenerTuplas (x:xs)
            | length (rankingC x) == 0 = obtenerTuplas xs
            | otherwise                = obtenerTupla x : obtenerTuplas xs
          obtenerTupla x               = (head (rankingC x), capacidadA (head (rankingC x))
                                                                        (fst (categoriaC x)))

-------------------------------------------------------------------------------
-- Fin de stevenBradburyJ -----------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- sequiaOlimpicaJ ------------------------------------------------------------
-------------------------------------------------------------------------------

sequiaOlimpicaJ :: JJOO -> [Pais]
sequiaOlimpicaJ j = buscarMasSecos (obtenerPaises (atletasJ j)) (obtenerPaises (atletasJ j))
    where buscarMasSecos [] _ = []
          buscarMasSecos [x] _ = [x]
          buscarMasSecos (x:xs) paises
              | esMasSeco x paises = x:(buscarMasSecos xs paises)
              | otherwise          = buscarMasSecos xs paises
          esMasSeco _ [] = True
          esMasSeco w (x:xs) = (maxDiasSinGanar w j >= maxDiasSinGanar x j) &&
                               (esMasSeco w xs)

obtenerPaises [] = []
obtenerPaises (x:xs) = (nacionalidadA x):sinRepetidos (obtenerPaises xs)

maxDiasSinGanar p j = buscarMax (calcularDiferencias (jornadas p j))
    where jornadas p j= 0 : (jornadasEnLasQueGano p j) ++ [jornadaActualJ j]

calcularDiferencias [x,y] = [y - x]
calcularDiferencias (x1:x2:xs) = (x2 - x1):(calcularDiferencias (x2:xs))

buscarMax [x] = x
buscarMax (x:xs)
    | esMax x xs = x
    | otherwise  = buscarMax xs
    where esMax _ [] = True
          esMax w (x:xs) = (w >= x) && (esMax w xs)

jornadasEnLasQueGano p j = acumularJornadasEnLasQueGano p 1 j
acumularJornadasEnLasQueGano p d j
    | d > jornadaActualJ j     = []
    | ganoMedallasEseDia p d j = d:(acumularJornadasEnLasQueGano p (d + 1) j)
    | otherwise                =    acumularJornadasEnLasQueGano p (d + 1) j

ganoMedallasEseDia p d j = ganoAlgunaMedalla p (filtrarFinalizadas (cronogramaJ j d))

filtrarFinalizadas [] = []
filtrarFinalizadas (x:xs)
    | finalizadaC x = x:(filtrarFinalizadas xs)
    | otherwise     = filtrarFinalizadas xs

ganoAlgunaMedalla p [] = False
ganoAlgunaMedalla p (x:xs) = (salioEnPosicion p 1 x  ||
                              salioEnPosicion p 2 x  ||
                              salioEnPosicion p 3 x) || ganoAlgunaMedalla p xs

salioEnPosicion pais pos c = length (rankingC c) >= pos &&
                             nacionalidadA (rankingC c !! (pos - 1)) == pais

-------------------------------------------------------------------------------
-- Fin de sequiaOlimpicaJ -----------------------------------------------------
-------------------------------------------------------------------------------
