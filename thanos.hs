--primera parte--

--------------
-- Punto 01 --
--------------

import Text.Show.Functions()

data Personaje = Personaje {
    nombre :: String,
    planeta :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [String]
} deriving(Show,Eq)

ironman = Personaje "Tony Stark" "Tierra" 45 100 ["supertraje"]
drStrange = Personaje "Stephen Strange" "Tierra" 40 80 ["magia", "controlar el tiempo"]
groot = Personaje "Groot" "Groot" 150 85 ["groot", "groot", "groot"]
viudaNegra = Personaje "Natasha" "Tierra" 35 90 ["combate cuerpo a cuerpo"]

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}deriving(Show)

type Gema = Personaje -> Personaje

gemaEjemplo :: Gema
gemaEjemplo = id

guanteEjemplo :: Guantelete
guanteEjemplo = Guantelete "uru" [
    gemaEjemplo,
    gemaEjemplo,
    gemaEjemplo,
    gemaEjemplo,
    gemaEjemplo,
    gemaEjemplo]


type Universo = [Personaje]

universito = [ironman, drStrange, groot]
universote = viudaNegra : universito

estanLasSeis :: Guantelete -> Bool
estanLasSeis unGuantelete = (length . gemas) unGuantelete == 6

snapear :: Guantelete -> Universo -> Universo
snapear unGuantelete unUniverso
    | estanLasSeis unGuantelete =  take (div (length unUniverso) 2) unUniverso
    | otherwise                 = unUniverso


--------------
-- Punto 02 --
--------------

aptoParaPendex :: Universo -> Bool
aptoParaPendex unUniverso = any ((<45) . edad) unUniverso

energiaTotal :: Universo -> Int
energiaTotal unUniverso = sum .(map energia) . (filter ((>1).length . habilidades)) $ unUniverso


-- Segunda Parte -- 

--------------
-- Punto 03 --
--------------

pierdeEnergia :: Int -> Personaje -> Personaje 
pierdeEnergia perdida unPersonaje = unPersonaje {energia = energia unPersonaje - perdida}

sacarHabilidad :: String -> Personaje -> Personaje
sacarHabilidad habilidad unPersonaje = unPersonaje { habilidades = filter (/=habilidad) (habilidades unPersonaje)}

sacarHabilidades :: Personaje -> Personaje
sacarHabilidades unPersonaje = unPersonaje {habilidades = []}

mente :: Int -> Gema
mente x unPersonaje = unPersonaje {energia = energia unPersonaje - x}

alma :: String -> Gema
alma habilidad = (pierdeEnergia 10) . (sacarHabilidad habilidad) 

espacio :: String -> Gema
espacio unPlaneta unPersonaje = (pierdeEnergia 20) unPersonaje {planeta = unPlaneta}

poder :: Gema
poder unPersonaje
    | (length . habilidades) unPersonaje <= 2 = (pierdeEnergia (energia unPersonaje)) . sacarHabilidades $ unPersonaje
    | otherwise = pierdeEnergia (energia unPersonaje) unPersonaje

tiempo :: Gema
tiempo unHeroe
    | edad unHeroe <= 18 = pierdeEnergia 50 unHeroe
    | edad unHeroe <= 37 = pierdeEnergia 50 unHeroe {edad = 18}
    | otherwise          = pierdeEnergia 50 unHeroe {edad = div (edad unHeroe) 2}

gemaLoca :: Gema -> Gema
gemaLoca unaGema = unaGema . unaGema

--------------
-- Punto 04 --
--------------

guantecito = Guantelete goma [tiempo, alma "usar Mjolnir", gemaLoca . (alma "programacion en Haskell")]

--------------
-- Punto 05 --
--------------

atacarConGema :: Gema -> Personaje -> Personaje
atacarConGema unaGema enemigo = unaGema enemigo

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldl atacarConGema enemigo gemas

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa (Guantelete _ [unaSolaGema]) enemigo = unaSolaGema
gemaMasPoderosa (Guantelete _ (x:y:xs))      enemigo
    | energia (x enemigo) < energia (y enemigo) = 
                  gemaMasPoderosa (Guantelete _ (x:xs)) enemigo
    | otherwise = gemaMasPoderosa (Guantelete _ (y:xs)) enemigo

--------------
-- Punto 07 --
--------------

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- El primer caso no se puede ejecutar ya que sin terminar 