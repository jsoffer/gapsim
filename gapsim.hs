{-

gapsim.hs
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

import Grafica
import Sesion

import Data.List as DL

import Text.ParserCombinators.Parsec
import Data.Set as Set

{-

SESIÓN DE ENTRADA
-----------------

La sesión de GAP debe ser preprocesada para ser adecuadamente analizada por este archivo.

Ese preproceso consiste en:

* reemplazar ';' por ' ; ' (no debe haber identificadores tocando ';'
* reemplazar ":=" por " := " (análogamente)
* agregar espacios alrededor de los '=' que no sean parte de ":=" 

Eso puede ser agregado a este programa, pero no existe actualmente. Este programa espera 
que la sesión de entrada ya tenga esas ayudas extra para el analizador.

-}

-- Declaración de tipos

data Memoria = Memoria { alloc :: (Set Objeto), memfree :: Integer } deriving (Show, Eq, Ord)

type Cola = [Objeto]
type Grafica = Either ParseError (Set Objeto)
type Sesion = [Identificador]

-- Una función que agregue un objeto a una cola,
type Encolamiento = Cola -> Objeto -> Cola 
-- después el tipo necesita un bool que diga si o no se aplica LRU, i.e. si restaura 
-- la cola en caso de invocación

-- misses, desalojos, total de bits transferidos
type Estadisticas = (Integer, Integer, Integer)

data Estado = Estado { stats :: Estadisticas, cola :: Cola, mem :: Memoria, cache :: Memoria } deriving (Show, Eq, Ord)

-- Informativo

mensaje :: Memoria -> String -> String
mensaje m x = if en_memoria m x then " Y" else " N"


-- ¿Existe un objeto con este nombre en la memoria m?
en_memoria :: Memoria -> String -> Bool
en_memoria m x = let res = DL.find (\k -> Grafica.nombre (vertice k) == x) $ elems $ alloc m in
    case res of
        Nothing -> False
        (Just _) -> True where

-- Muy bobo. Debe haber otra forma
-- Regresa el primer objeto de la gráfica con nombre s que encuentre
-- En realidad no debe haber más que uno en toda la gráfica
encuentra :: Set Objeto -> String -> Objeto
encuentra g s = if Prelude.null xs then error $ (show s)  ++ ": no existe en grafica" else head xs 
    where
        xs = Set.toList $ Set.filter (\k -> (Grafica.nombre $ vertice k) == s)  g

-- Gráfica y Encolamiento son estáticos (nunca cambian); Sesión solamente avanza.
-- Todos los cambios se almacenan en Estado.
proceso :: Estado -> Grafica -> Sesion -> Encolamiento -> Bool -> IO (Estado)
proceso _ (Left _) _ _ _ = error "El parse de la gráfica ha fallado"
proceso e _ [] _ _ = return e
proceso e (Right grafica) (x:xs) f lru = do
    -- debug; muestra cosas, no es tan complicado como parece.
    {-
    putStrLn $ 
        "- - -\n" ++ (show $ stats e) ++ "\n" ++
        (show $ Sesion.nombre x) ++ " " ++ 
        (show $ tam $ vertice $ encuentra grafica  (Sesion.nombre x)) ++ "/" ++ 
        (show $ memfree $ mem e) ++ " " ++ (show (accion x)) ++ 
        (mensaje (mem e) (Sesion.nombre x)) ++ "\n" ++ 
        ("memoria: " ++ (show $ (Set.map) ((Grafica.nombre).vertice) $ alloc $ mem e) ++ "\n") ++
        ("cache: " ++ (show $ (Set.map) ((Grafica.nombre).vertice) $ alloc $ cache e) ++ "\n") ++ 
        ("cola: " ++ (show $ Prelude.map ((Grafica.nombre).vertice) $ cola e)) ++ "\n - - "
    -}
    -- el motorcito
    res <- (case (accion x) of
        GENERA -> 
            -- Crea el nuevo identificador en memoria
            proceso (agregar e (Right grafica) x f lru) (Right grafica) xs f lru
        INVOCA -> 
            (if (en_memoria $ mem e) (Sesion.nombre x) 
                   -- sin LRU no pasa nada, avanza la sesión
                   -- con LRU actualiza la cola
                   then proceso (reestablece_en_cola e (Sesion.nombre x) lru) (Right grafica) xs f lru
                   -- sube del caché a la memoria el dato consultado 
                   else proceso (subir e (Right grafica) x f lru) (Right grafica) xs f lru) )
    return (res)

-- paso es parecido a proceso, pero con diferente función.
-- Regresa Estado, no IO (). No despliega, es solamente el motor.  
-- Se emplea para pasos intermedios en otras funciones que no 
-- operan dentro de IO (subir, agregar). 
-- No puede ser agregado a proceso para sustituir al motor, porque 
-- proceso se invoca a sí mismo, paso es solamente un paso.
paso :: Estado -> Grafica -> Identificador -> Encolamiento -> Bool -> Estado
paso e (Right grafica) x f lru = 
    case (accion x) of
        GENERA -> 
            agregar e (Right grafica) x f lru -- guarda el nuevo elemento
        INVOCA -> 
            (if (en_memoria $ mem e) (Sesion.nombre x)
                   -- sin LRU no pasa nada, avanza la sesión
                   -- con LRU actualiza la cola
                   then (reestablece_en_cola e (Sesion.nombre x) lru)
                   else subir e (Right grafica) x f lru) -- caché -> memoria

-- si LRU, entonces busca el objeto con nombre x en la cola, lo retira y lo pone al final
reestablece_en_cola :: Estado -> String -> Bool -> Estado
reestablece_en_cola e x lru =
    if lru 
    then Estado s q m c 
    else e where
        s = stats e
        q = case elemento of 
            Nothing ->  cola e
            Just _ -> (y:zs) ++ ys -- FIXME
        m = mem e
        c = cache e
        -- tal vez poner los dos no es necesario (¿son lo mismo?)
        elemento = find (\k -> x == (Grafica.nombre (vertice k))) (cola e)
        (zs, (y:ys)) = break (\k -> x == (Grafica.nombre (vertice k))) (cola e)

subiendo :: Integer -> Estadisticas -> Estadisticas
subiendo tam (a,b,c) = (a+1, b, c+tam)

bajando :: Integer -> Estadisticas -> Estadisticas
bajando tam (a,b,c) = (a, b+1, c+tam)

-- sube un objeto del caché a memoria principal
-- 
subir :: Estado -> Grafica -> Identificador -> Encolamiento -> Bool -> Estado 
subir e (Right grafica) y f lru =
    if (memfree $ mem e) >= (tam $ vertice x)  
    then Estado (subiendo bits (stats e)) q m c
    else paso (desalojar e) (Right grafica) y f lru where
        x = encuentra (alloc $ cache e) (Sesion.nombre y)
        -- si elemento == False, intentamos subir algo que no estáien caché. 
        -- Pero lo necesitamos. Salir.
        elemento = member x $ alloc $ cache e
        q = f (cola e) x -- encola con la función proporcionada
        bits = if elemento then tam $ vertice x else 0
        m = if elemento 
            then Memoria a s 
            else error $ (show x) ++ "no en cache, no puede subir" --mem e
        c = if elemento 
            then Memoria (Set.delete x $ alloc $ cache e) ((memfree $ cache e) + (tam $ vertice x)) 
            else error $ (show x) ++ "no en cache, no puede subir" --cache e  
        a = Set.insert x (alloc $ mem e) 
        s = ((memfree $ mem e) - (tam $ vertice x)) 

-- A partir de un estado, devuelve otro en el que, hipotéticamente, 
-- hay un poco más de memoria.
desalojar :: Estado -> Estado 
desalojar e = Estado (bajando bits (stats e)) q m c where
    x = last $ cola e
    q = init $ cola e -- (cuidado con la cola vacía)
    -- si elemento == False, entonces intentamos desalojar algo que no está en memoria. 
    -- Consecuencia de generar dos veces el mismo identificador. La primera vez lo agrega
    -- a memoria, la segunda no, pero ambas veces lo encola.
    -- El problema es, la gráfica solamente tiene una instancia del identificador, pero en
    -- el programa el tamaño podría cambiar.
    -- Supongamos que el tamaño no cambia (verificado en "datos"/cola), entonces es suficiente
    -- con ignorar. También puede ser manejado verificando que no encole algo que ya 
    -- existe (¿equivalente?).
    elemento = member x $ alloc $ mem e
    bits = if elemento then tam $ vertice x else 0
    m = if elemento 
        then Memoria (Set.delete x $ alloc $ mem e) ((memfree $ mem e) + (tam $ vertice x)) 
        else mem e
    c = if elemento 
        then Memoria a s 
        else cache e
    a = Set.insert x (alloc $ cache e) 
    s = ((memfree $ cache e) - (tam $ vertice x)) 

agregar :: Estado -> Grafica -> Identificador -> Encolamiento -> Bool -> Estado
agregar e (Right grafica) y f lru = let x = encuentra grafica (Sesion.nombre y) in
                if ((memfree (mem e)) >= (tam $ vertice x)) -- si cabe
                then
                    -- haz una memoria con el nuevo elemento
                    Estado (stats e)
                           (f (cola e) x) 
                           (Memoria (Set.insert x (alloc (mem e))) 
                                    ((memfree (mem e)) - (tam $ vertice x))) 
                                    (cache e) 
                -- si no, mueve algo de mem a cache, y corre el mismo comando.
                else paso (desalojar e) (Right grafica) y f lru

-- Los encolamientos agregan al principio de la lista y leen y desencolan del final.
-- Por lo tanto, agregar requiere tiempo constante, retirar tiempo lineal.
-- En la práctica ambos tiempos pueden ser constantes, con el costo de reducir la 
-- simplicidad del código fuente sustituyendo [] por Seq.

-- encolan siempre al principio; leer con "last", desencolar con "init".

-- Primero en entrar, primero en salir.
fifo :: Encolamiento
fifo xs x = (x:xs)

-- solo tamaño
disc_tam :: Encolamiento
disc_tam xs x = menores ++ (x:mayores) where
    (menores, mayores) = break (\k -> tam (vertice k) > tam (vertice x)) xs

-- solo profundidad (menos profundo primero)
disc_depth :: Encolamiento
disc_depth xs x = menores ++ (x:mayores) where
    (menores, mayores) = break (\k -> depth k < depth x) xs

-- primero por tamaño, luego por profundidad
disc_tam_depth :: Encolamiento
disc_tam_depth xs x = menores ++ (x:mayores) where
    (menores, mayores) = 
        break (\k -> tam (vertice k) >= tam (vertice x) &&
            if tam (vertice k) == tam (vertice x) 
            then depth k < depth x
            else True) xs

-- primero profundidad, luego tamaño
disc_depth_tam :: Encolamiento
disc_depth_tam xs x = menores ++ (x:mayores) where
    (menores, mayores) = 
        break (\k -> depth k <= depth x &&
            if depth k == depth x
            then tam (vertice k) > tam (vertice x) 
            else True) xs


run :: Encolamiento -> Bool -> IO (Estadisticas)
run f lru = do
    let
        -- los dos niveles de memoria.
        l0 = Memoria (Set.empty :: Set Objeto) 130 -- es espacio libre al iniciar
        l1 = Memoria (Set.empty :: Set Objeto) 5000
    grafica <- archivo_a_grafica "datos" -- grafica :: Either ParseError (Set Objeto)
    sesion <- archivo_a_sesion "rubik.gap" -- sesion :: [Identificador]
    resultado <- proceso (Estado (0,0,0) [] l0 l1) grafica sesion f lru
    --resultado <- proceso (Estado [] l0 l1) grafica sesion ordentam   
    return (stats resultado)
