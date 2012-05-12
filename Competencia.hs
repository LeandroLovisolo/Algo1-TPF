module Competencia (Competencia, nuevaC, categoriaC, participantesC,
                    finalizadaC, rankingC, lesTocoControlAntiDopingC,
                    leDioPositivoC, finalizarC, linfordChristieC,
                    gananLosMasCapacesC, sancionarTrampososC)

where

import Tipos
import Atleta

data Competencia = C Categoria
                 | Participar Atleta Competencia
                 | Finalizar [Int] [(Int, Bool)] Competencia

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

rankingC :: Competencia -> [Atleta]
rankingC (Finalizar ciaNumbers _ c) = atletasPorCiaNumber ciaNumbers c

atletasPorCiaNumber :: [Int] -> Competencia -> [Atleta]
atletasPorCiaNumber [] c = []
atletasPorCiaNumber (x:xs) c = (buscar x (participantesC c)) : atletasPorCiaNumber xs c
    where buscar ciaNumber (x:xs)
            | ciaNumber == ciaNumberA(x) = x
            | otherwise                  = buscar ciaNumber xs

lesTocoControlAntiDopingC :: Competencia -> [Atleta]
lesTocoControlAntiDopingC (Finalizar _ dopping c) = atletasPorCiaNumber (ciaNumbers dopping) c
    where ciaNumbers [] = []
          ciaNumbers ((n,_):xs) = n:(ciaNumbers xs)

leDioPositivoC :: Competencia -> Atleta -> Bool
leDioPositivoC (Finalizar _ dopping _) a = buscar dopping (ciaNumberA a)
    where buscar (x:xs) ciaNumber
            | fst x == ciaNumber = snd x
            | otherwise          = buscar xs ciaNumber

finalizarC :: Competencia -> [Int] -> [(Int, Bool)] -> Competencia
finalizarC compe posiciones dopping = Finalizar posiciones dopping compe

linfordChristieC :: Competencia -> Atleta -> Competencia
linfordChristieC (C cat) _ = C cat
linfordChristieC (Participar atle compe) atletaASacar
    | (ciaNumberA atle) /= (ciaNumberA atletaASacar) =
        Participar atle (linfordChristieC compe atletaASacar)
    | otherwise = (linfordChristieC compe atletaASacar)

sancionarTrampososC :: Competencia -> Competencia
sancionarTrampososC (Finalizar ranking dopping compe) =
    Finalizar (auxSinTramposos ranking dopping) dopping compe

auxSinTramposos :: [Int] -> [(Int, Bool)] -> [Int]
auxSinTramposos ranking [] = ranking
auxSinTramposos [] _ = []
auxSinTramposos (rank:ranking) dopping
    | elem rank (auxCiaDoppingVerdadero dopping) = auxSinTramposos ranking dopping
    | otherwise                                  = rank : (auxSinTramposos ranking dopping)

auxCiaDoppingVerdadero :: [(Int, Bool)] -> [Int] 
auxCiaDoppingVerdadero [] = []
auxCiaDoppingVerdadero (x:xs) | (snd x) == True = (fst x) : auxCiaDoppingVerdadero xs
                              | otherwise = auxCiaDoppingVerdadero xs

gananLosMasCapacesC :: Competencia -> Bool
gananLosMasCapacesC (Finalizar [] dopping compe) = True
gananLosMasCapacesC (Finalizar [x] dopping compe) = True
gananLosMasCapacesC (Finalizar (frank:srank:ranking) dopping compe) =
    (capacidadA (auxAtletaConCia frank (participantesC compe))
                (fst (categoriaC compe))) >=
    (capacidadA (auxAtletaConCia srank (participantesC compe))
                (fst (categoriaC compe))) &&
    gananLosMasCapacesC (Finalizar (srank:ranking) dopping compe)

auxAtletaConCia :: Int -> [Atleta] -> Atleta
auxAtletaConCia cia (atle:atletas) | (ciaNumberA atle) == cia = atle
                                   | otherwise = auxAtletaConCia cia atletas

instance Show Competencia where
    show c = "Competencia " ++ show (categoriaC c) ++ (participantes c) ++ (ranking c)
        where participantes c = if length (participantesC c) > 0
                                then ": " ++ show (participantesC c)
                                else "";
              ranking c = if finalizadaC c
                          then ", ranking: [" ++ ciaNumbers (rankingC c) ++ "]"
                          else "";
              ciaNumbers [x] = (show (ciaNumberA x));
              ciaNumbers (x:xs) = (show (ciaNumberA x)) ++ "," ++ ciaNumbers xs;
