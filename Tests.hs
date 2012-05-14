-------------------------------------------------------------------------------
-- Para correr los tests, cargar este módulo en la consola de Hugs           --
-- ejecutando el siguiente comando desde el directorio raíz del proyecto:    --
--                                                                           --
-- hugs -P{HUGS}/*:.:hunit Tests                                             --
--                                                                           --
-- Reemplazando {HUGS} por el directorio en donde se instalaron los paquetes --
-- de Hugs. Por ejemplo, si dicho directorio fuera /usr/lib/hugs/packages,   --
-- el comando a correr sería el siguiente:                                   --
--                                                                           --
-- hugs -P/usr/lib/hugs/packages/*:.:hunit Tests                             --
--                                                                           --
-- Una vez cargado el módulo en el intérprete, se pueden correr los tests    --
-- tipeando lo siguiente:                                                    --
--                                                                           --
-- runtests                                                                  --
--                                                                           --
-- Este comando imprime un reporte en la consola con el éxito o fracaso de   --
-- los tests.                                                                --
--                                                                           --
-- Los tests fueron implementados usando la librería HUnit. Más información  --
-- en la siguiente URL:                                                      --
--                                                                           --
-- http://hunit.sourceforge.net/HUnit-1.0/Guide.html                         --
-------------------------------------------------------------------------------


module Tests where

import HUnit

import Tipos
import Atleta
import Competencia
import JJOO


runtests = runTestTT tests


-------------------------------------------------------------------------------
-- Tests ----------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Sintaxis para agregar tests nuevos:
-- 
--   "nombre del test" :~ valorEsperado @=? expresion
--
-- Donde:
--   (*) "nombre del test" es un string que describe *brevemente* lo que
--       se intenta testear;
--   (*) valorEsperado es el valor que debería devolver la expresión testeada
--       para considerarse correcta;
--   (*) expresion es el código que nos interesa evaluar para luego comparar
--       el valor devuelto contra valorEsperado.
--
-- Ejemplo:
-- 
--   "la suma de 2 y 2 es 4" :~ 4 @=? (2+2)
--
-- En este ejemplo se intenta verificar que el resultado devuelto por (2+2)
-- sea en efecto 4, y llamamos al test "la suma de 2 y 2 es 4".


tests = TestList [

        -------------------------------------------------------------------------------
        -- Módulo Atleta --------------------------------------------------------------
        -------------------------------------------------------------------------------
        

        "entrenarDeporteA : agrega varios deportes y verifica orden" ~:
            True @=? deportesEnOrden (deportesA (entrenarDeportes (nuevoA "Pepe" Masculino 18 "Arg" 133)
                                                                  dataEntrenarDeporteA)),

        
        -------------------------------------------------------------------------------
        -- Módulo Competencia ---------------------------------------------------------
        -------------------------------------------------------------------------------

        --rankingC -------------------------------------------------------------------

        "rankingC: devuelve los atletas correctos" ~:
            ["Abel", "Beto", "Carlos", "Daniel", "Esteban", "Federico", "Gabriel", "Horacio"]
                @=? map nombreA (rankingC dataCompetencia),

        -- lesTocoControlAntiDopingC --------------------------------------------------

        "lesTocoControlAntiDopingC: devuelve los atletas correctos" ~:
            [222, 444] @=? map ciaNumberA (lesTocoControlAntiDopingC dataCompetencia),

        -- leDioPositivoC -------------------------------------------------------------

        "leDioPositivoC: devuelve los valores correctos" ~:
            [False, True] @=? map (\a -> leDioPositivoC dataCompetencia a)
                                  (lesTocoControlAntiDopingC dataCompetencia),

        -- sancionarTrampososC --------------------------------------------------------

        "sancionarTrampososC : saca del ranking a los que le dio positivo en el anti-dopping"~:
          [111, 222, 333, 555, 666, 777, 888] @=? map ciaNumberA (rankingC (sancionarTrampososC dataCompetencia)),

        "sancionarTrampososC : probando con un dopping vacio"~:
          [111, 222, 333, 444, 555, 666, 777, 888] @=? map ciaNumberA (rankingC (sancionarTrampososC dataCompetenciaSinDopping)),

        -- gananLosMasCapacesC --------------------------------------------------------

        "gananLosMasCapacesC : True si las capacidades de los atletas en el ranking son decrecientes"~:
          True @=? gananLosMasCapacesC dataGananLosMasCapacesC,

        "gananLosMasCapacesC : Devuelve falso por tener el ranking desordenado"~:
          False @=? gananLosMasCapacesC dataGananLosMasCapacesCFalso,

        "gananLosMasCapacesC : Devuelve True teniendo ranking vacio"~:
          True @=? gananLosMasCapacesC dataGananLosMasCapacesCVacio,


        -------------------------------------------------------------------------------
        -- Módulo JJOO ----------------------------------------------------------------
        -------------------------------------------------------------------------------


         -- dePaseoJ -------------------------------------------------------------------

        "dePaseoJ: devuelve los atletas correctos" ~:
            [555, 666] @=? map ciaNumberA (dePaseoJ dataDePaseoJ),

        -- transcurrirDiaJ ------------------------------------------------------------

        "transcurrirDiaJ: jornada actual correcta" ~:
            3 @=? jornadaActualJ (transcurrirDiaJ dataTranscurrirDiaJ),

        "transcurrirDiaJ: el día transcurrido tiene la cantidad de competencias correcta" ~:
            3 @=? length (cronogramaJ (transcurrirDiaJ dataTranscurrirDiaJ) 2),

        "transcurrirDiaJ: las competencias del día transcurrido fueron finalizadas" ~:
            True @=? all finalizadaC (cronogramaJ (transcurrirDiaJ dataTranscurrirDiaJ) 2),

        "transcurrirDiaJ: las competencias de los demas días no se modificaron" ~:
            True @=? (map finalizadaC (cronogramaJ dataTranscurrirDiaJ 1)) ==
                     (map finalizadaC (cronogramaJ (transcurrirDiaJ dataTranscurrirDiaJ) 1)),

        "transcurrirDiaJ: ranking Volley correcto" ~:
            [555, 222, 444, 111, 666, 888, 777, 333] @=?
                map ciaNumberA (rankingC ((cronogramaJ (transcurrirDiaJ dataTranscurrirDiaJ) 2) !! 0)),

        "transcurrirDiaJ: ranking Arqueria correcto" ~:
            [999, 888, 777, 666, 555, 444, 333, 222, 111] @=?
                map ciaNumberA (rankingC ((cronogramaJ (transcurrirDiaJ dataTranscurrirDiaJ) 2) !! 1)),

        "transcurrirDiaJ: ranking Natacion correcto" ~:
            [999, 888, 777, 666, 555, 444, 333, 222, 111] @=?
                map ciaNumberA (rankingC ((cronogramaJ (transcurrirDiaJ dataTranscurrirDiaJ) 2) !! 2)),

        "transcurrirDiaJ: control antidoping a un atleta de cada competencia no finalizada" ~:
            [0,1,1] @=? map (\c -> length (lesTocoControlAntiDopingC c))
                            (cronogramaJ (transcurrirDiaJ dataTranscurrirDiaJ) 2),

        -- medalleroJ -----------------------------------------------------------------

        "medalleroJ: devuelve los valores correctos" ~:
            [("Argentina", [2, 1, 0]),
             ("Ecuador",   [1, 1, 0]),
             ("Chile",     [1, 0, 2]),
             ("Brasil",    [0, 2, 1]),
             ("Dinamarca", [0, 0, 1])] @=? medalleroJ dataMedalleroJ,

        -- uyOrdenadoAsiHayUnPatronJ -----------------------------------------------

        "uyOrdenadoAsiHayUnPatronJ: existe patrón" ~:
            True @=? uyOrdenadoAsiHayUnPatronJ dataUyOrdenadoAsiHayUnPatronJ,

        "uyOrdenadoAsiHayUnPatronJ: no existe patrón" ~:
            False @=? uyOrdenadoAsiHayUnPatronJ dataUyOrdenadoAsiHayUnPatronJ',

        -- boicotPorDisciplinaJ ---------------------------------------------------------

        "boicotPorDisciplinaJ: el país boicoteado tiene atletas" ~:
            True @=? probarBoicotPorDisciplinaJ dataBoicotPorDisciplinaJ ("Futbol", Masculino) "Argentina",
        
        "boicotPorDisciplinaJ:  el país boicoteado no tiene atletas" ~:
            True @=? probarBoicotPorDisciplinaJ dataBoicotPorDisciplinaJ ("Futbol", Masculino) "Etiopia",

        -- stevenBradburyJ ------------------------------------------------------------

        "stevenBradburyJ: una sola competencia" ~:
            "Abel" @=? nombreA (stevenBradburyJ dataStevenBradburyJ),

        "stevenBradburyJ: dos competencias" ~:
            "Beto" @=? nombreA (stevenBradburyJ dataStevenBradburyJ'),

        "stevenBradburyJ: dos stevens" ~:
            "Carlos" @=? nombreA (stevenBradburyJ dataStevenBradburyJ''),

        -- losMasFracasadosJ ----------------------------------------------------------

        "losMasFracasadosJ" ~: True @=?
            mismosAtletas [dataAtleta "Abel"     "Argentina" 1 [],
                           dataAtleta "Beto"     "Argentina" 2 []]
                          (losMasFracasadosJ dataLosMasFracasadosJ "Argentina"),

        -- liuSongJ -------------------------------------------------------------------

        "liuSongJ: " ~: True @=? probarLiuSongJ,

        -- sequiaOlimpicaJ ------------------------------------------------------------

        "sequiaOlimpicaJ: juegos vacío" ~:
            [] @=? sequiaOlimpicaJ (nuevoJ 2012 [] []),

        "sequiaOlimpicaJ: 1 día, 1 país, 0 competencias" ~:
            ["Argentina"] @=? sequiaOlimpicaJ (nuevoJ 2012 [dataAtleta "Juan" "Argentina" 1 []] []),

        "sequiaOlimpicaJ: 1 día, 2 paises, 0 competencias" ~:
            ["Argentina", "Brasil"] @=?
                sequiaOlimpicaJ (nuevoJ 2012 [dataAtleta "Juan" "Argentina" 1 [],
                                              dataAtleta "Pepe" "Brasil"    2 []] []),

        "sequiaOlimpicaJ: 1 día, 2 paises, 1 competencia, 1 medallista" ~:
            ["Argentina", "Brasil"] @=? sequiaOlimpicaJ dataSequiaOlimpicaJ,

        "sequiaOlimpicaJ: 3 días, 2 paises, 1 seco" ~:
            ["Brasil"] @=? sequiaOlimpicaJ dataSequiaOlimpicaJ',

        "sequiaOlimpicaJ: 5 días, 3 paises, 2 secos" ~:
            ["Brasil", "Chile"] @=? sequiaOlimpicaJ dataSequiaOlimpicaJ''
    ]


-------------------------------------------------------------------------------
-- Datos de prueba ------------------------------------------------------------
-------------------------------------------------------------------------------


-- General --------------------------------------------------------------------

dataCompetencia :: Competencia
dataCompetencia = finalizarC (nuevaC "Futbol" Masculino dataAtletas)
                             [111, 222, 333, 444, 555, 666, 777, 888]
                             [(222, False), (444, True)]

dataAtletas :: [Atleta]
dataAtletas = map entrenarDeportes atletas 
  where atletas = [(nuevoA "Abel"     Masculino 18 "Argentina" 111),
                   (nuevoA "Beto"     Masculino 19 "Brasil"    222),
                   (nuevoA "Carlos"   Masculino 20 "Chile"     333),
                   (nuevoA "Daniel"   Masculino 21 "Dinamarca" 444),
                   (nuevoA "Esteban"  Masculino 22 "Ecuador"   555),
                   (nuevoA "Federico" Masculino 23 "Francia"   666),
                   (nuevoA "Gabriel"  Masculino 24 "Grecia"    777),
                   (nuevoA "Horacio"  Masculino 25 "Honduras"  888),
                   (nuevoA "Hector"   Masculino 26 "Honduras"  999)]
        deportes = ["Futbol", "Handball", "Basket", "Volley", "Arqueria", "Natacion"]
        entrenarDeportes atleta = foldl entrenarDeporte atleta deportes
        entrenarDeporte atleta deporte = entrenarDeporteA atleta deporte (capacidad atleta)
        capacidad atleta = ciaNumberA atleta

dataEntrenarDeporteA :: [(Deporte, Int)]
dataEntrenarDeporteA = [("Futbol", 23), ("Bmx", 40), ("Tenis", 60), ("Trekking", 30), ("Buceo", 23), ("Javalina", 30)]

entrenarDeportes :: Atleta -> [(Deporte, Int)] -> Atleta
entrenarDeportes atle [] = atle
entrenarDeportes atle (dep:deportes) = entrenarDeporteA atle (fst dep) (snd dep)

deportesEnOrden :: [Deporte] -> Bool
deportesEnOrden [] = True
deportesEnOrden [x] = True
deportesEnOrden (fdep:sdep:deportes) = (fdep <= sdep) && (deportesEnOrden (sdep:deportes))

dataAtleta :: String -> Pais -> Int -> [(Deporte, Int)] -> Atleta
dataAtleta nombre pais ciaNumber capacidades =
        entrenar (nuevoA nombre Masculino 18 pais ciaNumber) capacidades
    where entrenar a []     = a
          entrenar a (x:xs) = entrenar (entrenarDeporteA a (fst x) (snd x)) xs

transcurrirDias j 0 = j
transcurrirDias j d = transcurrirDias (transcurrirDiaJ j) (d - 1)

-- sancionarTrampososC --------------------------------------------------------

dataCompetenciaSinDopping :: Competencia
dataCompetenciaSinDopping = finalizarC (nuevaC "Futbol" Masculino dataAtletas)
                                       [111, 222, 333, 444, 555, 666, 777, 888] []

-- gananLosMasCapacesC --------------------------------------------------------

dataGananLosMasCapacesC :: Competencia
dataGananLosMasCapacesC = finalizarC (nuevaC "Futbol" Masculino atletas) [3,2,1] []
    where atletas    = [dataAtleta "Juan" "Argentina" 1 [("Futbol", 10), ("Basket", 40), ("Handball", 50)],
                        dataAtleta "Pepe" "Brasil"    2 [("Futbol", 20), ("Basket", 30), ("Handball", 60)],
                        dataAtleta "Pepe" "Brasil"    3 [("Futbol", 60), ("Basket", 30), ("Handball", 60)]]

dataGananLosMasCapacesCFalso :: Competencia
dataGananLosMasCapacesCFalso = finalizarC (nuevaC "Futbol" Masculino atletas) [1,2,3] []
    where atletas    = [dataAtleta "Juan" "Argentina" 1 [("Futbol", 10), ("Basket", 40), ("Handball", 50)],
                        dataAtleta "Pepe" "Brasil"    2 [("Futbol", 20), ("Basket", 30), ("Handball", 60)],
                        dataAtleta "Pepe" "Brasil"    3 [("Futbol", 60), ("Basket", 30), ("Handball", 60)]]

dataGananLosMasCapacesCVacio :: Competencia
dataGananLosMasCapacesCVacio = finalizarC (nuevaC "Futbol" Masculino atletas) [] []
    where atletas    = [dataAtleta "Juan" "Argentina" 1 [("Futbol", 10), ("Basket", 40), ("Handball", 50)],
                        dataAtleta "Pepe" "Brasil"    2 [("Futbol", 20), ("Basket", 30), ("Handball", 60)],
                        dataAtleta "Pepe" "Brasil"    3 [("Futbol", 60), ("Basket", 30), ("Handball", 60)]]

-- dePaseoJ -------------------------------------------------------------------

dataDePaseoJ :: JJOO
dataDePaseoJ = nuevoJ 2012 atletas cronograma
    where atletas = atletasActivos ++ atletasDePaseo
          atletasActivos = [(nuevoA "Abel"     Masculino 18 "Argentina" 111),
                            (nuevoA "Beto"     Masculino 19 "Brasil"    222),
                            (nuevoA "Carlos"   Masculino 20 "Chile"     333),
                            (nuevoA "Daniel"   Masculino 21 "Dinamarca" 444)]
          atletasDePaseo = [(nuevoA "Esteban"  Masculino 22 "Ecuador"   555),
                            (nuevoA "Federico" Masculino 22 "Francia"   666)]
          cronograma = [dia1, dia2]
          dia1 = [(competencia "Futbol"),
                  (competencia "Handball"),
                  (competencia "Basket")]
          dia2 = [(competencia "Volley"),
                  (competencia "Arqueria"),
                  (competencia "Natacion")]
          competencia d = (nuevaC d Masculino atletasActivos)

-- medalleroJ -----------------------------------------------------------------

dataMedalleroJ :: JJOO
dataMedalleroJ = dataTranscurrirDiaJ

-- transcurrirDiaJ ------------------------------------------------------------

dataTranscurrirDiaJ :: JJOO
dataTranscurrirDiaJ = transcurrirDiaJ (nuevoJ 2012 dataAtletas cronograma)
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
          competenciaF dep pos = finalizarC (nuevaC dep Masculino dataAtletas) pos []
          competencia  dep     = (nuevaC dep Masculino dataAtletas)

-- uyOrdenadoAsiHayUnPatronJ --------------------------------------------------

dataUyOrdenadoAsiHayUnPatronJ :: JJOO
dataUyOrdenadoAsiHayUnPatronJ = (nuevoJ 2012 dataAtletas cronograma)
    where cronograma = [dia1, dia2, dia3, dia4]
          dia1 = [(competenciaF "Futbol"             [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competenciaF "Handball"           [333, 111, 222, 888, 555, 444, 777, 666]),
                  (competenciaF "Basket"             [111, 555, 333, 444, 888, 222, 666, 777])]
          dia2 = [(competenciaF "Volley"             [555, 222, 444, 111, 666, 888, 777, 333]),
                  (competencia  "Arqueria"),
                  (competencia  "Natacion")]
          dia3 = [(competenciaF "Gimnasia Artistica" [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competencia  "Hockey"),
                  (competencia  "Rugby")]
          dia4 = [(competenciaF "Snowboard"          [555, 222, 444, 111, 666, 888, 777, 333]),
                  (competencia  "Skate"),
                  (competencia  "BMX")]
          competenciaF dep pos = finalizarC (nuevaC dep Masculino dataAtletas) pos []
          competencia  dep     = (nuevaC dep Masculino dataAtletas)

dataUyOrdenadoAsiHayUnPatronJ' :: JJOO
dataUyOrdenadoAsiHayUnPatronJ' = (nuevoJ 2012 dataAtletas cronograma)
    where cronograma = [dia1, dia2, dia3, dia4]
          dia1 = [(competenciaF "Futbol"             [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competenciaF "Handball"           [333, 111, 222, 888, 555, 444, 777, 666]),
                  (competenciaF "Basket"             [111, 555, 333, 444, 888, 222, 666, 777])]
          dia2 = [(competenciaF "Volley"             [555, 222, 444, 111, 666, 888, 777, 333]),
                  (competencia  "Arqueria"),
                  (competencia  "Natacion")]
          dia3 = [(competenciaF "Gimnasia Artistica" [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competencia  "Hockey"),
                  (competencia  "Rugby")]
          dia4 = [(competenciaF "Snowboard"          [111, 555, 333, 444, 888, 222, 666, 777]),
                  (competencia  "Skate"),
                  (competencia  "BMX")]
          competenciaF dep pos = finalizarC (nuevaC dep Masculino dataAtletas) pos []
          competencia  dep     = (nuevaC dep Masculino dataAtletas)

-- boicotPorDisciplinaJ -------------------------------------------------------

probarBoicotPorDisciplinaJ j c p       = seEliminanLosCorrectos && cantidadCorrecta
    where seEliminanLosCorrectos       = not (hayAtletasDelPais p (competencia (competencias (snd result) (cantDiasJ (snd result))) c))
          cantidadCorrecta             = length (buscarBoicoteados (participantesC (competenciaBoicoteada (competencias j 1)))) == fst result
          buscarBoicoteados []         = []
          buscarBoicoteados (x:xs)
            | nacionalidadA x == p     = x:(buscarBoicoteados xs)
            | otherwise                = buscarBoicoteados xs
          competenciaBoicoteada (x:xs)
            | categoriaC x == c        = x
            | otherwise                = competenciaBoicoteada xs
          result                       = boicotPorDisciplinaJ j c p
          competencias j' 1            =  cronogramaJ j' 1
          competencias j' x            = (cronogramaJ j' x) ++ (competencias j' (x-1))
          competencia (x:xs) c
            | categoriaC x == c        = x
            | otherwise                = competencia xs c
          obtenerPaises []             = []
          obtenerPaises (x:xs)         = (nacionalidadA x) : (obtenerPaises xs)
          hayAtletasDelPais p c        = elem p (obtenerPaises (participantesC c))

dataBoicotPorDisciplinaJ :: JJOO
dataBoicotPorDisciplinaJ = (nuevoJ 2012 dataAtletas cronograma)
    where cronograma = [dia1, dia2, dia3, dia4]
          dia1 = [(competenciaF "Futbol"   [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competenciaF "Handball" [333, 111, 222, 888, 555, 444, 777, 666]),
                  (competenciaF "Basket"   [111, 555, 333, 444, 888, 222, 666, 777])]
          dia2 = [(competenciaF "Volley"   [555, 222, 444, 111, 666, 888, 777, 333]),
                  (competencia  "Arqueria"),
                  (competencia  "Natacion")]
          dia3 = [(competenciaF "Gimnasia Artistica" [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competencia  "Hockey"),
                  (competencia  "Rugby")]
          dia4 = [(competenciaF "Snowboard" [111, 555, 333, 444, 888, 222, 666, 777]),
                  (competencia  "Skate"),
                  (competencia  "Bmx")]
          competenciaF dep pos = finalizarC (nuevaC dep Masculino dataAtletas) pos []
          competencia  dep     = (nuevaC dep Masculino dataAtletas)

-- stevenBradburyJ ------------------------------------------------------------

dataStevenBradburyJ   = nuevoJ 2012 atletas [[competencia]]
    where atletas     = [dataAtleta "Abel" "Argentina" 1 [("Futbol", 100)]]
          competencia = finalizarC (nuevaC "Futbol" Masculino atletas) [1] []

dataStevenBradburyJ'   = nuevoJ 2012 atletas [competencias]
    where atletas      = [dataAtleta "Abel" "Argentina" 1 [("Futbol", 100), ("Basket", 50)],
                          dataAtleta "Beto" "Argentina" 2 [("Futbol", 80),  ("Basket", 20)]]
          competencias = [finalizarC (nuevaC "Futbol" Masculino atletas) [1] [],
                          finalizarC (nuevaC "Basket" Masculino atletas) [2] []]

dataStevenBradburyJ'' = transcurrirDiaJ (nuevoJ 2012 atletas [dia1, dia2])
    where atletas     = [dataAtleta "Abel"   "Argentina" 1 [("Futbol", 100), ("Basket", 50), ("Handball", 40)],
                         dataAtleta "Beto"   "Argentina" 2 [("Futbol", 80),  ("Basket", 20), ("Handball", 30)],
                         dataAtleta "Carlos" "Argentina" 3 [("Futbol", 90),  ("Basket", 60), ("Handball", 10)]]
          dia1        = [finalizarC (nuevaC "Futbol"   Masculino atletas) [1,2,3] [],
                         finalizarC (nuevaC "Basket"   Masculino atletas) [2,1,3] []]
          dia2        = [finalizarC (nuevaC "Handball" Masculino atletas) [3,1,2] []]

-- sequiaOlimplicaJ -----------------------------------------------------------

dataSequiaOlimpicaJ   = nuevoJ 2012 atletas [[competencia]]
    where atletas     = [dataAtleta "Juan" "Argentina" 1 [("Futbol", 10)],
                         dataAtleta "Pepe" "Brasil"    2 [("Futbol", 20)]]
          competencia = finalizarC (nuevaC "Futbol" Masculino atletas) [1] []

dataSequiaOlimpicaJ' = transcurrirDias (nuevoJ 2012 atletas [dia1, dia2, dia3]) 2
    where atletas    = [dataAtleta "Juan" "Argentina" 1 [("Futbol", 10), ("Basket", 40), ("Handball", 50)],
                        dataAtleta "Pepe" "Brasil"    2 [("Futbol", 20), ("Basket", 30), ("Handball", 60)]]
          dia1       = [finalizarC (nuevaC "Futbol"   Masculino atletas) [1]   []]
          dia2       = [finalizarC (nuevaC "Basket"   Masculino atletas) [1]   []]
          dia3       = [finalizarC (nuevaC "Handball" Masculino atletas) [2,1] []]

dataSequiaOlimpicaJ'' = transcurrirDias (nuevoJ 2012 atletas [dia1, dia2, dia3, dia4, dia5]) 4
    where atletas     = [dataAtleta "Juan"   "Argentina" 1 [("A", 10), ("B", 40), ("C", 50), ("D", 80), ("E", 90)],
                         dataAtleta "Pepe"   "Brasil"    2 [("A", 20), ("B", 30), ("C", 60), ("D", 70), ("E", 95)],
                         dataAtleta "Carlos" "Chile"     3 [("A", 20), ("B", 30), ("C", 60), ("D", 70), ("E", 95)]]
          dia1        = [finalizarC (nuevaC "A" Masculino atletas) [2,1] []]
          dia2        = [finalizarC (nuevaC "B" Masculino atletas) [1,2] []]
          dia3        = [finalizarC (nuevaC "C" Masculino atletas) [3,1] []]
          dia4        = [finalizarC (nuevaC "D" Masculino atletas) [1]   []]
          dia5        = [finalizarC (nuevaC "E" Masculino atletas) [2,3] []]

-- liuSongJ -------------------------------------------------------------------

dataLiuSongJ       = transcurrirDiaJ (nuevoJ 2012 atletas [dia1, dia2, dia3])
    where atleta1  = dataAtleta "Abel"     "Argentina" 1 [("A", 10), ("B", 80)]
          atleta2  = dataAtleta "Beto"     "Brasil"    2 [("A", 20), ("B", 70)]
          atleta3  = dataAtleta "Coco"     "Chile"     3 [("A", 30), ("B", 60)]
          atleta4  = dataAtleta "Dani"     "Dinamarca" 4 [("A", 40), ("B", 50)]
          atleta5  = dataAtleta "Eber"     "Ecuador"   5 [("A", 50), ("B", 40)]
          atleta6  = dataAtleta "Fede"     "Finlandia" 6 [("A", 60), ("B", 30)]
          atleta7  = dataAtleta "Gabi"     "Grecia"    7 [("A", 70), ("B", 20)]
          liu      = dataAtleta "Liu Song" "China"     8 [("A", 80), ("B", 10)]
          atletas  = [atleta1, atleta2, atleta3, atleta4, atleta5, atleta6, atleta7, liu]
          atletas1 = [atleta1, atleta2, liu]
          atletas2 = [atleta3, atleta4, atleta5]
          atletas3 = [atleta6, atleta7, liu]
          dia1     = [(finalizarC (nuevaC "A" Masculino atletas1) [1,2] [(1, False)]),
                      (finalizarC (nuevaC "B" Masculino atletas2) [2,1] [(3, True)])]
          dia2     = [nuevaC "C" Masculino atletas3]
          dia3     = [nuevaC "D" Masculino atletas1,
                      nuevaC "E" Masculino atletas3]

probarLiuSongJ    = mismosAtletas (atletasJ result) atletas
    where result  = liuSongJ dataLiuSongJ
                            (dataAtleta "Liu Song" "Chile" 8 [("A", 80), ("B", 10)])
                             "Argentina"
          atletas = [dataAtleta "Abel"     "Argentina" 1 [("A", 10), ("B", 80)],
                     dataAtleta "Beto"     "Brasil"    2 [("A", 20), ("B", 70)],
                     dataAtleta "Coco"     "Chile"     3 [("A", 30), ("B", 60)],
                     dataAtleta "Dani"     "Dinamarca" 4 [("A", 40), ("B", 50)],
                     dataAtleta "Eber"     "Ecuador"   5 [("A", 50), ("B", 40)],
                     dataAtleta "Fede"     "Finlandia" 6 [("A", 60), ("B", 30)],
                     dataAtleta "Gabi"     "Grecia"    7 [("A", 70), ("B", 20)],
                     dataAtleta "Liu Song" "Argentina" 8 [("A", 80), ("B", 10)]]


mismosAtletas as1 as2 = all (\a1 -> any (\a2 -> atletasIguales a2 a1) as2) as1 &&
                        all (\a2 -> any (\a1 -> atletasIguales a1 a2) as1) as2

atletasIguales a1 a2 = nombreA a1         == nombreA a2         &&
                       sexoA a1           == sexoA a2           &&
                       anioNacimientoA a1 == anioNacimientoA a2 &&
                       nacionalidadA a1   == nacionalidadA a2   &&
                       ciaNumberA a1      == ciaNumberA a2      &&
                       mismos (deportesA a1) (deportesA a2)     &&
                       mismos (tuplasDepCap a1) (tuplasDepCap a2)
    where tuplasDepCap a = map (\d -> (d, capacidadA a d)) (deportesA a)

mismos :: Eq a => [a] -> [a] -> Bool
mismos x y = (all (`elem` x) y) && (all (`elem` y) x)

-- losMasFracasadosJ ----------------------------------------------------------

dataLosMasFracasadosJ = transcurrirDias (nuevoJ 2012 atletas [dia1, dia2]) 2
    where atletas     = [atleta1, atleta2, atleta3, atleta4,
                         atleta5, atleta6, atleta7, atleta8] 
          atleta1     = dataAtleta "Abel"     "Argentina" 1 []
          atleta2     = dataAtleta "Beto"     "Argentina" 2 []
          atleta3     = dataAtleta "Coco"     "Argentina" 3 []
          atleta4     = dataAtleta "Dani"     "Argentina" 4 []
          atleta5     = dataAtleta "Eber"     "Argentina" 5 []
          atleta6     = dataAtleta "Fede"     "Brasil"    6 []
          atleta7     = dataAtleta "Gabi"     "Brasil"    7 []
          atleta8     = dataAtleta "Lalo"     "Brasil"    8 []
          atletasA    = atletas
          atletasB    = [atleta1, atleta2, atleta3, atleta4, atleta6, atleta8]
          atletasC    = [atleta1, atleta2, atleta3, atleta5, atleta7, atleta8]
          dia1        = [finalizarC (nuevaC "A" Masculino atletasA) [3,5,6,7,8] [],
                         finalizarC (nuevaC "B" Masculino atletasB) [8,4,6,2,1] []]
          dia2        = [finalizarC (nuevaC "C" Masculino atletasC) [7,8,5,2,3] []]
