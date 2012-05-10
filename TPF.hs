module TPF where

import Tipos
import Atleta
import Competencia
import JJOO

--testJuegos = NuevoDia [(Participar (A "reloco" Masculino 1991 "arg" 2 [("football", 12)]) (C ("football", Masculino)))] (NuevoDia [(Participar (A "pepe" Masculino 1991 "arg" 1 [("tenis",50)]) (C ("tenis", Masculino)))] (J 12 [(A "pepe" Masculino 1991 "arg" 1 [("tenis", 50)]), (A "reloco" Masculino 1991 "arg" 2 [("football", 12)]), (A "pepe" Masculino 1991 "arg" 3 [])] 1))
--testCompetencia = (Finalizar [01,02,03,04,05] [(02,True),(04,False)] (Participar (A "atl1" Masculino 1990 "arg" 01 [("futbol",10)]) (Participar (A "atl2" Masculino 1990 "arg2" 02 [("futbol",10)]) (Participar (A "atl3" Masculino 1990 "arg3" 03 [("futbol",10)]) (Participar (A "atl4" Masculino 1990 "arg4" 04 [("futbol",10)]) (Participar (A "atl5" Masculino 1990 "arg5" 05 [("futbol",10)]) (C ("futbol",Masculino))))))))
--testAtleta = (A "pepe" Masculino 1991 "arg" 1 [("futbol",20),("basquet",50)])
--testAtletas = [(A "pepe" Masculino 1991 "arg" 1 []), (A "pepa" Masculino 1991 "arg" 2 []), (A "pepo" Masculino 1991 "arg" 3 [])]
--testAtletasx = [(A "pepe" Masculino 1991 "arg" 1 []), (A "pepa" Masculino 1991 "arg" 2 [])]
-- Datos de prueba. Devuelve una lista de competencias finalizadas.
testCompetencias :: [Competencia]
testCompetencias = [(competencia "Futbol" [333, 111, 222, 555, 444]),
                   (competencia "Basket" [111, 555, 333, 444, 222]),
                   (competencia "Volley" [555, 222, 444, 111, 333])]
	where competencia deporte posiciones =
		finalizarC (nuevaC deporte Masculino testAtletas) posiciones []

-- Datos de prueba. Devuelve una lista de atletas entrenados en ciertos deportes.
testAtletas :: [Atleta]
testAtletas = map entrenarDeportes atletas 
  where atletas = [(nuevoA "Abel"    Masculino 18 "Argentina" 111),
                   (nuevoA "Beto"    Masculino 19 "Brasil"    222),
                   (nuevoA "Carlos"  Masculino 20 "Chile"     333),
                   (nuevoA "Daniel"  Masculino 21 "Dinamarca" 444),
                   (nuevoA "Esteban" Masculino 22 "Ecuador"   555)]
        deportes = ["Futbol", "Basket", "Volley"]
        entrenarDeportes atleta = foldl entrenarDeporte atleta deportes
        entrenarDeporte atleta deporte = entrenarDeporteA atleta deporte 10