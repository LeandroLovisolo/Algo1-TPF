module Competencia (Competencia(..), nuevaC, categoriaC, participantesC,
                    finalizadaC, rankingC, lesTocoControlAntiDopingC,
                    leDioPositivoC, finalizarC, linfordChristieC,
                    gananLosMasCapacesC, sancionarTrampososC)

where

import Tipos
import Atleta

data Competencia = C Categoria
                 | Participar Atleta Competencia
                 | Finalizar [Int] [(Int, Bool)] Competencia
                   deriving (Show)

nuevaC :: Deporte -> Sexo -> [Atleta] -> Competencia
nuevaC dep sex [] = C (dep, sex)
nuevaC dep sex (atle:atletas) = Participar atle (nuevaC dep sex atletas)

categoriaC :: Competencia -> Categoria
categoriaC (C categoria)  = categoria
categoriaC (Participar _ compe) = categoriaC compe
categoriaC (Finalizar _ _ compe) = categoriaC compe

participantesC :: Competencia -> [Atleta]
participantesC (C _) = []
participantesC (Participar atle compe) = atle : (participantesC compe)
participantesC (Finalizar _  _ compe) = participantesC compe

finalizadaC :: Competencia -> Bool
finalizadaC (Finalizar _ _ _) = True
finalizadaC _ = False

auxAletasConCia :: [Int] -> [Atleta] -> [Atleta]
auxAletasConCia _ [] = []
auxAletasConCia ciaNumbers (atle:atletas) | elem (ciaNumberA atle) ciaNumbers = atle : (auxAletasConCia ciaNumbers atletas)
										  | otherwise = auxAletasConCia ciaNumbers atletas
auxCiaDoppingVerdadero :: [(Int, Bool)] -> [Int] 
auxCiaDoppingVerdadero [] = []
auxCiaDoppingVerdadero (x:xs) | (snd x) == True = (fst x) : auxCiaDoppingVerdadero xs
							  | otherwise = auxCiaDoppingVerdadero xs

rankingC :: Competencia -> [Atleta]
rankingC (Finalizar ciaNumbers _ compe) = auxAletasConCia ciaNumbers (participantesC compe)

lesTocoControlAntiDopingC :: Competencia -> [Atleta]
lesTocoControlAntiDopingC (Finalizar _ dopping compe) = auxAletasConCia (auxCiaDoppingVerdadero dopping) (participantesC compe)

leDioPositivoC :: Competencia -> Atleta -> Bool
leDioPositivoC (Finalizar _ dopping _) atle = elem (ciaNumberA atle) (auxCiaDoppingVerdadero dopping)

finalizarC :: Competencia -> [Int] -> [(Int, Bool)] -> Competencia
finalizarC compe posiciones dopping = Finalizar posiciones dopping compe

linfordChristieC :: Competencia -> Atleta -> Competencia
linfordChristieC (C cat) _ = C cat
linfordChristieC (Participar atle compe) atletaASacar | (ciaNumberA atle) /= (ciaNumberA atletaASacar) = Participar atle (linfordChristieC compe atletaASacar)
													  | otherwise = (linfordChristieC compe atletaASacar)

auxSinTramposos :: [Int] -> [(Int, Bool)] -> [Int]
auxSinTramposos (rank:ranking) dopping | elem rank (auxCiaDoppingVerdadero dopping) = auxSinTramposos ranking dopping
									   | otherwise = rank : (auxSinTramposos ranking dopping)

sancionarTrampososC :: Competencia -> Competencia
sancionarTrampososC (Finalizar ranking dopping compe) = Finalizar (auxSinTramposos ranking dopping) dopping compe

gananLosMasCapacesC = undefined
--gananLosMasCapacesC :: Competencia -> Bool
--gananLosMasCapacesC (Finalizar [x] dopping compe) = True
--gananLosMasCapacesC (Finalizar (frank:srank:ranking) dopping compe) = (capacidadA ((auxAletasConCia frank)!!0) (fst (categoriaC compe)) >= capacidadA ((auxAletasConCia srank)!!0) (fst (categoriaC compe))) && gananLosMasCapacesC (Finalizar ranking dopping compe)