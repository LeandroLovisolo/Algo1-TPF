module Atleta (Atleta, nuevoA, nombreA, sexoA, anioNacimientoA,
               nacionalidadA, ciaNumberA, deportesA, capacidadA,
               entrenarDeporteA)
where

import Tipos

data Atleta = A String Sexo Int Pais Int [(Deporte, Int)] deriving (Show)

nuevoA :: String -> Sexo -> Int -> Pais -> Int -> Atleta
nuevoA nom s a nac cia = (A nom s a nac cia [])

nombreA :: Atleta -> String
nombreA (A nombre _ _ _ _ _) = nombre

sexoA :: Atleta -> Sexo
sexoA (A _ sexo _ _ _ _) = sexo

anioNacimientoA :: Atleta -> Int
anioNacimientoA (A _ _ nacimiento _ _ _) = nacimiento

nacionalidadA :: Atleta -> Pais
nacionalidadA (A _ _ _ pais _ _) = pais

ciaNumberA :: Atleta -> Int
ciaNumberA (A _ _ _ _ ciaNumber _) = ciaNumber

deportesA :: Atleta -> [(Deporte, Int)]
deportesA (A _ _ _ _ _ deportes) = deportes

capacidadA :: Atleta -> Deporte -> Int
capacidadA (A _ _ _ _ _ (dep:deps)) deporte | (fst dep) == deporte = (snd dep)

auxExisteDeporte :: [(Deporte, Int)] -> Deporte -> Bool
auxExisteDeporte [] _ = False
auxExisteDeporte (x:xs) dep | (fst x) == dep = True
							| otherwise = auxExisteDeporte xs dep

auxModificaCapacidad :: [(Deporte, Int)] -> Deporte -> Int -> [(Deporte, Int)]
auxModificaCapacidad [] _ _ = []
auxModificaCapacidad (deporte:ldeportes) depAModificar capAModificar | (fst deporte) == depAModificar = ((fst deporte), capAModificar) : (auxModificaCapacidad ldeportes depAModificar capAModificar)
																	 | otherwise = deporte : (auxModificaCapacidad ldeportes depAModificar capAModificar)

--auxAgregarDeporte :: [(Deporte, Int)] -> Deporte -> Int -> [(Deporte, Int)]
entrenarDeporteA :: Atleta -> Deporte -> Int -> Atleta
entrenarDeporteA (A nombre sexo anio pais cia deportes) depPorAgregar capPorAgregar | auxExisteDeporte deportes depPorAgregar = (A nombre sexo anio pais cia (auxModificaCapacidad deportes depPorAgregar capPorAgregar) )
--																					| otherwise = (A nombre sexo anio pais cia (auxAgregarDeporte deportes depPorAgregar capPorAgregar))