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

        "medalleroJ: devuelve los valores correctos" ~:
            [("Argentina", [2, 1, 0]),
             ("Ecuador",   [1, 1, 0]),
             ("Chile",     [1, 0, 2]),
             ("Brasil",    [0, 2, 1]),
             ("Dinamarca", [0, 0, 1])] @=? medalleroJ dataMedalleroJ
             
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
