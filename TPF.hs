module TPF where

import Tipos
import Atleta
import Competencia
import JJOO

testJuegos = NuevoDia [(Participar (A "reloco" Masculino 1991 "arg" 2 []) (C ("deporte", Masculino)))] (NuevoDia [(Participar (A "pepe" Masculino 1991 "arg" 1 []) (C ("deporte", Masculino)))] (J 12 [(A "pepe" Masculino 1991 "arg" 1 [])] 1))
