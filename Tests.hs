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
        -- Módulo Competencia ---------------------------------------------------------
        -------------------------------------------------------------------------------


        "rankingC: devuelve los atletas correctos" ~:
            ["Abel", "Beto", "Carlos", "Daniel", "Esteban", "Federico", "Gabriel", "Horacio"]
                @=? map nombreA (rankingC dataCompetencia),

        "lesTocoControlAntiDopingC: devuelve los atletas correctos" ~:
            [222, 444] @=? map ciaNumberA (lesTocoControlAntiDopingC dataCompetencia),

        "leDioPositivoC: devuelve los valores correctos" ~:
            [False, True] @=? map (\a -> leDioPositivoC dataCompetencia a)
                                  (lesTocoControlAntiDopingC dataCompetencia),


        -------------------------------------------------------------------------------
        -- Módulo JJOO ----------------------------------------------------------------
        -------------------------------------------------------------------------------


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

        -- stevenBradburyJ ------------------------------------------------------------

        "stevenBradburyJ: una sola competencia" ~:
            "Abel" @=? nombreA (stevenBradburyJ dataStevenBradburyJ),

        "stevenBradburyJ: dos competencias" ~:
            "Beto" @=? nombreA (stevenBradburyJ dataStevenBradburyJ'),

        "stevenBradburyJ: dos stevens" ~:
            "Carlos" @=? nombreA (stevenBradburyJ dataStevenBradburyJ''),

        -- uyOrdenadoAsiHayUnPatronJ -----------------------------------------------

        "uyOrdenadoAsiHayUnPatronJ : devuelve True para dataUyOrdenadoAsiHayUnPatronJ" ~:
          True @=? uyOrdenadoAsiHayUnPatronJ dataUyOrdenadoAsiHayUnPatronJ,

        "uyOrdenadoAsiHayUnPatronJ : devuelve False para dataUyOrdenadoAsiHayUnPatronJDos" ~:
          False @=? uyOrdenadoAsiHayUnPatronJ dataUyOrdenadoAsiHayUnPatronJDos,

        -- boicotPorDisciplinaJ ---------------------------------------------------------

        "boicotPorDisciplinaJ : devuelve JJOO sin atletas de ese pais en la categoria" ~:
          False @=? auxExisteNacionalidad
          (todasLasCompe (boicotPorDisciplinaJ dataBoicotPorDisciplinaJ ("Futbol",Masculino) "Argentina") 
            (cantDiasJ (boicotPorDisciplinaJ dataBoicotPorDisciplinaJ ("Futbol",Masculino) "Argentina" ))) "Argentina" ("Futbol",Masculino),
        
        "boicotPorDisciplinaJ : devuelve JJOO sin atletas de ese pais en la categoria, prueba con ningun atleta de tal pais en la categoria" ~:
          False @=? auxExisteNacionalidad
          (todasLasCompe (boicotPorDisciplinaJ dataBoicotPorDisciplinaJ ("Futbol",Masculino) "Etiopia") 
            (cantDiasJ (boicotPorDisciplinaJ dataBoicotPorDisciplinaJ ("Futbol",Masculino) "Etiopia" ))) "Etiopia" ("Futbol",Masculino)

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


dataCompetencia :: Competencia
dataCompetencia = finalizarC (nuevaC "Futbol" Masculino dataAtletas)
                             [111, 222, 333, 444, 555, 666, 777, 888]
                             [(222, False), (444, True)]

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

dataMedalleroJ :: JJOO
dataMedalleroJ = dataTranscurrirDiaJ

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

dataUyOrdenadoAsiHayUnPatronJ :: JJOO
dataUyOrdenadoAsiHayUnPatronJ = (nuevoJ 2012 dataAtletas cronograma)
    where cronograma = [dia1, dia2, dia3, dia4]
          dia1 = [(competenciaF "Futbol"   [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competenciaF "Handball" [333, 111, 222, 888, 555, 444, 777, 666]),
                  (competenciaF "Basket"   [111, 555, 333, 444, 888, 222, 666, 777])]
          dia2 = [(competenciaF "Volley"   [555, 222, 444, 111, 666, 888, 777, 333]),
                  (competencia  "Arqueria"),
                  (competencia  "Natacion")]
          dia3 = [(competenciaF  "Gimnasia Artistica" [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competencia  "Hockey"),
                  (competencia  "Rugby")]
          dia4 = [(competenciaF "Snowboard" [555, 222, 444, 111, 666, 888, 777, 333]),
                  (competencia  "Skate"),
                  (competencia  "Bmx")]
          competenciaF dep pos = finalizarC (nuevaC dep Masculino dataAtletas) pos []
          competencia  dep     = (nuevaC dep Masculino dataAtletas)

dataUyOrdenadoAsiHayUnPatronJDos :: JJOO
dataUyOrdenadoAsiHayUnPatronJDos = (nuevoJ 2012 dataAtletas cronograma)
    where cronograma = [dia1, dia2, dia3, dia4]
          dia1 = [(competenciaF "Futbol"   [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competenciaF "Handball" [333, 111, 222, 888, 555, 444, 777, 666]),
                  (competenciaF "Basket"   [111, 555, 333, 444, 888, 222, 666, 777])]
          dia2 = [(competenciaF "Volley"   [555, 222, 444, 111, 666, 888, 777, 333]),
                  (competencia  "Arqueria"),
                  (competencia  "Natacion")]
          dia3 = [(competenciaF  "Gimnasia Artistica" [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competencia  "Hockey"),
                  (competencia  "Rugby")]
          dia4 = [(competenciaF "Snowboard" [111, 555, 333, 444, 888, 222, 666, 777]),
                  (competencia  "Skate"),
                  (competencia  "Bmx")]
          competenciaF dep pos = finalizarC (nuevaC dep Masculino dataAtletas) pos []
          competencia  dep     = (nuevaC dep Masculino dataAtletas)


dataBoicotPorDisciplinaJ :: JJOO
dataBoicotPorDisciplinaJ = (nuevoJ 2012 dataAtletas cronograma)
    where cronograma = [dia1, dia2, dia3, dia4]
          dia1 = [(competenciaF "Futbol"   [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competenciaF "Handball" [333, 111, 222, 888, 555, 444, 777, 666]),
                  (competenciaF "Basket"   [111, 555, 333, 444, 888, 222, 666, 777])]
          dia2 = [(competenciaF "Volley"   [555, 222, 444, 111, 666, 888, 777, 333]),
                  (competencia  "Arqueria"),
                  (competencia  "Natacion")]
          dia3 = [(competenciaF  "Gimnasia Artistica" [111, 222, 333, 555, 444, 777, 888, 666]),
                  (competencia  "Hockey"),
                  (competencia  "Rugby")]
          dia4 = [(competenciaF "Snowboard" [111, 555, 333, 444, 888, 222, 666, 777]),
                  (competencia  "Skate"),
                  (competencia  "Bmx")]
          competenciaF dep pos = finalizarC (nuevaC dep Masculino dataAtletas) pos []
          competencia  dep     = (nuevaC dep Masculino dataAtletas)

auxExisteNacionalidad :: [Competencia] -> Pais -> Categoria -> Bool
auxExisteNacionalidad [] _ _ = False
auxExisteNacionalidad (compe:competencias) pais cate 
              | ((categoriaC compe) == cate) && (elem pais (listaPaisesAtletas (participantesC compe) )) = True
              | otherwise = auxExisteNacionalidad competencias pais cate

listaPaisesAtletas :: [Atleta] -> [Pais]
listaPaisesAtletas [] = []
listaPaisesAtletas (atle:atletas) = (nacionalidadA atle) : (listaPaisesAtletas atletas)

todasLasCompe :: JJOO -> Int -> [Competencia]
todasLasCompe juegos 1 = cronogramaJ juegos 1
todasLasCompe juegos x = (cronogramaJ juegos x) ++ (todasLasCompe juegos (x-1))

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


dataAtleta :: String -> Pais -> Int -> [(Deporte, Int)] -> Atleta
dataAtleta nombre pais ciaNumber capacidades =
        entrenar (nuevoA nombre Masculino 18 pais ciaNumber) capacidades
    where entrenar a []     = a
          entrenar a (x:xs) = entrenar (entrenarDeporteA a (fst x) (snd x)) xs

transcurrirDias j 0 = j
transcurrirDias j d = transcurrirDias (transcurrirDiaJ j) (d - 1)
