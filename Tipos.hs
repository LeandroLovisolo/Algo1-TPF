module Tipos where

type Deporte = String
type Pais = String
type Categoria = (Deporte, Sexo)

data Sexo = Femenino | Masculino deriving (Show, Eq)