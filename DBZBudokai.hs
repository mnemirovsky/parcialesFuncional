import Text.Show.Functions
import Data.List

-- Funciones que tal vez te pueden servir, tal vez no

-- Main*> :t takeWhile
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- Main*> takeWhile even [2,4,6,5,6,7,8,9]
-- [2,4,6]

-- Main*> :t genericLength
-- genericLength :: Num i => [a] -> i
-- Main*> genericLength [2,4,6,5,6,7,8,9]
-- 8

-- Parcial DBZ Budokai Tenkaichi 3 --

--------------
-- Punto 01 --
--------------

data GuerreroZ = GuerreroZ {
    nombre       :: String,
    nivelKi      :: Float,
    raza         :: String,
    cansancio    :: Float,
    personalidad :: String
} deriving(Show)

gohan :: GuerreroZ
gohan = GuerreroZ "Gohan" 10000 "Saijayin" 0 "perezoso"

--------------
-- Punto 02 --
--------------

esPoderoso:: GuerreroZ -> Bool
esPoderoso unGuerrero = ((> 8000) . nivelKi $ unGuerrero) || ((== "Saiyajin") . raza $ unGuerrero)


--------------
-- Punto 03 --
--------------

type Ejercicio = GuerreroZ -> GuerreroZ

pressDeBanca :: Ejercicio
pressDeBanca  = (subirCansancio (100)) . (subirKi (90))

flexiones :: Ejercicio
flexiones = subirCansancio 50

saltosAlCajon :: Int -> Ejercicio
saltosAlCajon centimetros = (subirKi (fromIntegral(div centimetros 10))) . (subirCansancio (fromIntegral(div centimetros 5)))

snatch :: Ejercicio
snatch unGuerrero 
    | esExperimentado unGuerrero = (subirKi (nivelKi unGuerrero * 0.05)) . (subirCansancio (cansancio unGuerrero * 0.1)) $ unGuerrero
    | otherwise                  = subirCansancio 100 unGuerrero

ejercitar :: GuerreroZ -> Ejercicio -> GuerreroZ
ejercitar unGuerrero unEjercicio 
    | nivelesDeCansancio unGuerrero == "fresco"   = unEjercicio unGuerrero
    | nivelesDeCansancio unGuerrero == "cansado"  = ejercitarCansado unEjercicio unGuerrero
    | nivelesDeCansancio unGuerrero == "exhausto" = ejercitarExhausto unGuerrero

--auxiliares 
subirCansancio :: Float -> GuerreroZ -> GuerreroZ
subirCansancio valor unGuerrero = unGuerrero {cansancio = cansancio unGuerrero + valor}

subirKi :: Float -> GuerreroZ -> GuerreroZ
subirKi valor unGuerrero = unGuerrero { nivelKi = nivelKi unGuerrero + valor}

esExperimentado :: GuerreroZ -> Bool
esExperimentado = (>= 22000) . nivelKi


nivelesDeCansancio :: GuerreroZ -> String
nivelesDeCansancio unGuerrero 
    | cansancio unGuerrero > nivelKi unGuerrero * 0.72 = "exhausto"
    | cansancio unGuerrero > nivelKi unGuerrero * 0.44 = "cansado"
    | otherwise                                        = "fresco"

ejercitarCansado :: Ejercicio -> GuerreroZ -> GuerreroZ
ejercitarCansado unEjercicio unGuerrero = 
    (subirKiCansado unEjercicio) . (subirCansancioCansado unEjercicio) $ unGuerrero

subirKiCansado :: Ejercicio -> GuerreroZ -> GuerreroZ
subirKiCansado unEjercicio unGuerrero = 
    subirKi (2 * (nivelKi (unEjercicio unGuerrero) - nivelKi unGuerrero)) unGuerrero

subirCansancioCansado :: Ejercicio -> GuerreroZ -> GuerreroZ
subirCansancioCansado unEjercicio unGuerrero = 
    subirCansancio (4 * (cansancio (unEjercicio unGuerrero) - cansancio unGuerrero)) unGuerrero

ejercitarExhausto :: GuerreroZ -> GuerreroZ
ejercitarExhausto unGuerrero = perderKi unGuerrero
 where
    perderKi = subirKi (-0.02 * nivelKi unGuerrero)

--------------
-- Punto 04 --
--------------
type Rutina = [Ejercicio]

armarRutina :: Rutina -> GuerreroZ -> Rutina
armarRutina unosEjercicios unGuerrero
    | personalidad unGuerrero == "sacado"   = unosEjercicios
    | personalidad unGuerrero == "perezoso" = map agregarDescanso5 unosEjercicios
    | personalidad unGuerrero == "tramposo" = [id]

-- auxiliares
agregarDescanso5 :: Ejercicio -> Ejercicio
agregarDescanso5 unEjercicio unGuerrero = descansar 5 (unEjercicio unGuerrero)

--pregunta infinitos : no explícitamente. Podríamos definir una lista infinita de ejercicios que 
--arranque con flexiones, press de banca y luego una serie infinita de saltos al cajón, cada vez 10 
--cm más alto que la vez anterior. La podemos definir pero no terminaría de mostrarnos la lista nunca.

--------------
-- Punto 05 --
--------------

realizarRutina :: Rutina -> GuerreroZ -> GuerreroZ
realizarRutina unaRutina unGuerrero = foldl ejercitar unGuerrero (armarRutina unaRutina unGuerrero)

--------------
-- Punto 06 --
--------------

descansar :: Float -> GuerreroZ -> GuerreroZ
descansar minutos unGuerrero
    | cansancio (disminuirCansancio minutos unGuerrero) <= 0 = 
        unGuerrero { cansancio = 0 }
    | otherwise                                              = 
        disminuirCansancio minutos unGuerrero  

--auxiliares
disminuirCansancio :: Float -> GuerreroZ -> GuerreroZ
disminuirCansancio cantidad = subirCansancio (- energiaDescansada cantidad) 

energiaDescansada :: Float -> Float
energiaDescansada 1 = 1
energiaDescansada x = x + energiaDescansada (x - 1)

--------------
-- Punto 07 --
--------------

descansoOptimo :: GuerreroZ -> Int
descansoOptimo unGuerrero = 
    minutosOptimos(filter ((==0) . fst) (tuplaCansancioMinutos unGuerrero))

--auxiliares
tuplaCansancioMinutos :: GuerreroZ -> [(Float , Int)]
tuplaCansancioMinutos unGuerrero = zip (listaDeCansancio (listaDeGuerreroDescansando unGuerrero)) [0..]

listaDeGuerreroDescansando :: GuerreroZ -> [GuerreroZ]
listaDeGuerreroDescansando unGuerrero = iterate (+1) (descansar 1 unGuerrero)

listaDeCansancio :: [GuerreroZ] -> [Float]
listaDeCansancio unosGuerreros = map cansancio unosGuerreros

minutosOptimos :: (GuerreroZ, Float) -> Float
minutosOptimos = snd