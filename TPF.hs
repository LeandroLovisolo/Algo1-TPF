module TPF where

import Tipos
import Atleta
import Competencia
import JJOO

testJuegos = NuevoDia [(Participar (A "reloco" Masculino 1991 "arg" 2 [("football", 12)]) (C ("football", Masculino)))] (NuevoDia [(Participar (A "pepe" Masculino 1991 "arg" 1 [("tenis",50)]) (C ("tenis", Masculino)))] (J 12 [(A "pepe" Masculino 1991 "arg" 1 [("tenis", 50)]), (A "reloco" Masculino 1991 "arg" 2 [("football", 12)]), (A "pepe" Masculino 1991 "arg" 3 [])] 1))
testCompetencia = (Finalizar [01,02,03,04,05] [(02,True),(04,False)] (Participar (A "atl1" Masculino 1990 "arg" 01 [("futbol",10)]) (Participar (A "atl2" Masculino 1990 "arg2" 02 [("futbol",10)]) (Participar (A "atl3" Masculino 1990 "arg3" 03 [("futbol",10)]) (Participar (A "atl4" Masculino 1990 "arg4" 04 [("futbol",10)]) (Participar (A "atl5" Masculino 1990 "arg5" 05 [("futbol",10)]) (C ("futbol",Masculino))))))))
testAtleta = (A "pepe" Masculino 1991 "arg" 1 [("futbol",20),("basquet",50)])
testAtletas = [(A "pepe" Masculino 1991 "arg" 1 []), (A "pepa" Masculino 1991 "arg" 2 []), (A "pepo" Masculino 1991 "arg" 3 [])]
testAtletasx = [(A "pepe" Masculino 1991 "arg" 1 []), (A "pepa" Masculino 1991 "arg" 2 [])]
