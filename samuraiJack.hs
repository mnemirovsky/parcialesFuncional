import Text.Show.Functions()

data Elemento = UnElemento { 
	tipo :: String,
	ataque :: Personaje-> Personaje,
	defensa :: Personaje-> Personaje } deriving (Show)
data Personaje = UnPersonaje { 
	salud :: Float,
	elementos :: [Elemento],
	anioPresente :: Int } deriving (Show)

--------------
-- Punto 01 --
--------------

--a.
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio unAnio unPersonaje = unPersonaje {anioPresente = unAnio}

--auxiliar
cambiarSalud :: (Float -> Float) -> Personaje -> Personaje
cambiarSalud f unPersonaje = unPersonaje {salud = f (salud unPersonaje)}

--b.
meditar :: Personaje -> Personaje
meditar = cambiarSalud (*1.5) 

--c.
causarDanio :: Float -> Personaje -> Personaje
causarDanio danio unPersonaje 
    | salud unPersonaje - danio < 0 = cambiarSalud (const 0) unPersonaje
	| otherwise                     = cambiarSalud ((-)danio) unPersonaje

--------------
-- Punto 02 --
--------------

--a.
esMalvado :: Personaje -> Bool
esMalvado unPersonaje = elem "Maldad" (map tipo (elementos unPersonaje))

--b.
danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje unElemento
    | salud ((ataque unElemento) unPersonaje) < 0 = salud unPersonaje
	| otherwise = 
		salud unPersonaje - salud ((ataque unElemento) unPersonaje)

--c.
enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales heroe enemigos =
	filter (enemigoMortal heroe) enemigos

--auxiliares
elementoLetalContra :: Personaje -> Elemento -> Bool
elementoLetalContra heroe elemento =
	danioQueProduce heroe elemento == salud heroe
	
enemigoMortal :: Personaje -> Personaje -> Bool
enemigoMortal heroe enemigo = 
	any (elementoLetalContra heroe) (elementos enemigo)


--------------
-- Punto 03 --
--------------

--a.
concentracion :: Int -> Elemento
concentracion nivel = UnElemento "Magia" id (concentrar nivel)

--b.
esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = ((take cantidad) . repeat) esbirro
 where
	esbirro = UnElemento "Maldad" (causarDanio 1) id

--c.
jack :: Personaje
jack = UnPersonaje 300 [concentracion 3, katana] 200
 where 
	katana = UnElemento "Magia"  (causarDanio 1000) id

--d.
aku :: Int -> Float -> Personaje
aku anioAku saludAku = UnPersonaje saludAku ((concentracion 4) : portalAFuturo anioAku : (esbirrosMalvados (anioAku * 100))) anioAku

--auxiliares

concentrar :: Int -> Personaje -> Personaje
concentrar 1 personaje = meditar personaje
concentrar x personaje = concentrar (x-1) (meditar personaje)

portalAFuturo :: Int -> Elemento
portalAFuturo unAnio = UnElemento{
	tipo = "Magia",
	ataque = mandarAlAnio (unAnio + 2800),
	defensa = aku (unAnio + 2800) . salud
}

--------------
-- Punto 04 --
--------------

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
	| salud atacante == 0 = (defensor, atacante)
	| otherwise = luchar defensorTrasRonda atacanteTrasRonda
	where
		defensorTrasRonda = aplicarAtaques atacante defensor
		atacanteTrasRonda = aplicarDefensivos atacante

--auxiliar

aplicarAtaques :: Personaje -> Personaje -> Personaje
aplicarAtaques atacante defensor =
	foldr (($) . ataque) defensor (elementos atacante)  

aplicarDefensivos :: Personaje -> Personaje
aplicarDefensivos atacante = 
	foldr (($) . defensa) atacante (elementos atacante)

--------------
-- Punto 05 --
--------------
f :: Num d => (a , c) -> (b -> c) -> c 
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))
