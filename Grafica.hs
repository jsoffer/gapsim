module Grafica where

import Data.Set as Set

import Text.ParserCombinators.Parsec
import System.Environment

-- los nombres están completamente mal. Además, el formato de salida podría mejorar.
data Relacion = Relacion { desde :: String, hasta :: String } deriving (Show, Ord, Eq)
data Vertice = Vertice { nombre :: String, tam :: Integer } deriving (Show, Ord, Eq)

-- mal nombre (renombrar)
data Objeto = Objeto { vertice :: Vertice, antecesores :: Set String } deriving (Show, Ord, Eq)

encabezado :: Parser ()
encabezado = do
    string "digraph "
    manyTill alphaNum (char ' ')
    string "{\n"
    return ()

pie :: Parser ()
pie = do
    char '}'
    newline
    return ()

par :: Parser Relacion
par = do
    desde <- manyTill alphaNum (char '_')
    string "0x"
    manyTill hexDigit (char ' ')
    string "-> "
    hasta <- manyTill alphaNum (char '_')
    string "0x"
    manyTill hexDigit (string " \n")
    return $ Relacion desde hasta

lista_pares :: Parser ([Relacion])
lista_pares = do
    r <- many par
    return r

elemento :: Parser Vertice
elemento = do
    nombre <- manyTill alphaNum (char ' ')
    string "0x"
    direccion <- manyTill hexDigit (char ' ')
    tam <- manyTill hexDigit (char ' ')
    manyTill hexDigit (char ' ')
    manyTill hexDigit (char ' ')
    manyTill hexDigit (char ' ')
    manyTill hexDigit (char ' ')
    manyTill (char ' ') newline
    return $ Vertice nombre (read tam)

s_cola :: Parser [Vertice]
s_cola = do
    string "(cola"
    manyTill (char ' ') newline
    r <- manyTill elemento (char ')')
    return r

archivo :: Parser (Set Relacion, Set Vertice)
archivo = do
    encabezado
    r <- lista_pares
    pie
    s <- s_cola
    manyTill anyChar eof
    return (Set.fromList r, Set.fromList s)

-- El conjunto de vértices contiene la información de cada nodo. El conjunto de
-- relaciones indica por nombre cómo se relacionan los nodos. Si un nodo no se 
-- encuentra en el conjunto de relaciones entonces está solo. 
--
-- La función 'objetos' construye un conjunto que asocia a cada nodo con el conjunto
-- de nodos que son ancestros a dicho nodo.

objetos :: (Set Relacion, Set Vertice) -> Set Objeto
objetos (rels, verts) = fromList lista_objetos where
    lista_objetos = [ Objeto v a | 
        v <- (elems verts), 
        let a = Set.map desde $ Set.filter (\k -> nombre v == hasta k ) rels ]

-- la entrada es el nombre del archivo
archivo_a_grafica :: String -> IO (Either ParseError (Set Objeto))
archivo_a_grafica x = do
    f <- readFile x
    let tr = parse archivo "" f
    return $ case tr of
        (Left x) -> Left x
        (Right x) -> Right (objetos x)
