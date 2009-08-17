{-

Sesion.hs
---------

Copyright (c) 2009, Jaime Soffer
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Sesion where

-- Requiere de dos cosas: capturar los identificadores según se crean (para almacenarlos
-- en memoria) y según se utilizan (para hacerlos subir de un nivel más bajo si no 
-- están en el nivel principal).

import Debug.Trace

import Text.ParserCombinators.Parsec
import System.Environment

data Accion = GENERA | INVOCA deriving (Show, Eq)
data Identificador = Identificador { nombre :: String, accion :: Accion } deriving Show

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
    | f x = splitOn f xs
    | otherwise = let (h,t) = break f l in h:(splitOn f t)

es_alfanum :: Char -> Bool
es_alfanum x = elem x (['a'..'z']++['A'..'Z']++['0'..'9'])

-- En la sesión no debe haber comentarios, los identificadores empiezan con 
-- minúscula, y no hay identificadores pegados a un :=, :, =, o ;
--
-- Se debe revisar la sesión a mano antes de comenzar.

analiza :: String -> [String]
analiza = splitOn (\k -> not ((es_alfanum k) || elem k ";:=\""))

-- Si encuentra un identificador (no es vacío y empieza con minúscula) construye
-- un Identificador y lo agrega a la lista de salida. Si el elemento siguiente 
-- es un ":=" entonces lo marca como GENERA, en otro caso como INVOCA.
--
-- Hay que tener cuidado especialmente en que los INVOCA aparezcan antes que el 
-- GENERA al extraerlos de la misma expresión, i.e. antes de un ";".

crea :: [String] -> [Identificador]
crea l =  concat final where
    final = map construye parcial
    parcial = sublistas filtrado
    -- "in" es palabra clave
    filtrado = filter (\(k:ks) -> (k:ks) /= "in" && (elem k ['a'..'z']) || (k:ks) == ";" || (k:ks) == ":=") nonulos
    nonulos = filter (not.null) l
    sublistas :: [String] -> [[String]]
    sublistas = splitOn (==";")
    construye :: [String] -> [Identificador]
    construye [] = []
    construye (x:":=":xs) = (construye xs)++[(Identificador x GENERA)]
    construye (x:xs) = (Identificador x INVOCA):(construye xs)

muestra :: [Identificador] -> String
muestra [] = ""
muestra (x:xs) = ((nombre x) ++ " " ++ (show (accion x)) ++ "\n") ++ (muestra xs)

archivo_a_sesion :: String -> IO [Identificador]
archivo_a_sesion x = do
    f <- readFile x
    let tr = crea $ analiza f
    return tr

-- creo que la única forma de encadenar un Monad IO es que el destino sea IO
muestraM :: [Identificador] -> IO ()
muestraM xs = do
    let foo [] = ""
        foo (x:xs) = ((nombre x) ++ " " ++ (show (accion x)) ++ "\n") ++ (muestra xs)
        tr = xs
    putStrLn $ foo tr
