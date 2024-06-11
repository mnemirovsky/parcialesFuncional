--- Parcial Power Rangers ---

-------------------------- Punto 01 ----------------------------------------

--a. 
data Persona = Persona {
    habilidades :: [String],
    esBueno :: Bool
}deriving(Show, Eq)

type SinonimoPersona = (String, [String], Bool)

ejemploPersona = Persona ["bailar"] True

--b.
data PowerRanger = PowerRanger {
    color :: String,
    habilidades' :: [String],
    nivel :: Int
} deriving(Show, Eq)

type SinonimoPowerRanger = (String, [String], Float)

ejemploPowerRanger = PowerRanger "Verde" ["Pelear", "Llamar Megazord"] 100


-------------------------- Punto 02 ----------------------------------------

convertirEnPowerRanger :: String -> Persona -> PowerRanger
convertirEnPowerRanger color unaPersona = PowerRanger color (superHabilidades (habilidades unaPersona)) (nivelDePelea (habilidades unaPersona))

--auxiliares

superHabilidades :: [String] -> [String]
superHabilidades unasHabilidades =
    map ("Super" ++) unasHabilidades

nivelDePelea :: [String] -> Int
nivelDePelea unasHabilidades = sum . (map length) $ unasHabilidades

-------------------------- Punto 03 -------------------------------

jason = Persona ["Fuerza"] True
skull = Persona ["No-Muerte"] False
kimberly = Persona ["Velocidad", "Cebar Mates"] True
bulk = Persona ["Bailar cheto", "Poner la leche antes que los cereales"] False


formarEquipoRanger :: [String] -> [Persona] -> [PowerRanger]
formarEquipoRanger colores unosPersonajes = 
    zipWith (convertirEnPowerRanger) colores (filter esBueno unosPersonajes)

------------------------ Punto 04 ----------------------------------
--a. 
findOrElse :: Eq a => (a -> Bool) -> a -> [a] -> a
findOrElse condicion valor lista
    | filter condicion lista == [] = valor
    | otherwise = head . (filter condicion) $ lista

--b.
rangerLider :: [PowerRanger] -> PowerRanger
rangerLider equipo = findOrElse ((== "rojo") . color) (head equipo) equipo 

--------------------------------- Punto 05 ----------------------------------
--a.
maximumBy :: Ord a => (b -> a) -> [b] -> b
maximumBy funcion [x] = x
maximumBy funcion (x:y:xs) 
    | (funcion x) > (funcion y) = maximumBy funcion (x:xs)
    | otherwise = maximumBy funcion (y:xs)

--b.
rangerMasPoderoso :: [PowerRanger] -> PowerRanger
rangerMasPoderoso equipo = maximumBy nivel equipo


---------------------- Punto 06 -------------------

rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso = (> 5) . length . habilidades'

---------------------- Punto 07 -------------------

--a.
alfa5 = PowerRanger "Metalico" ["Reparar cosas" , infinitosAy] 0

-- auxiliar 
infinitosAy :: String 
infinitosAy = "ay " ++ infinitosAy

--b. 
-- caso que si termina: rangerHabilidoso alfa5
-- caso que no termina: superHabilidades (habilidades' alfa5)