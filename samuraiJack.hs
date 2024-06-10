import Text.Show.Functions()

data Elemento = UnElemento { 
	tipo :: String,
	ataque :: (Personaje-> Personaje),
	defensa :: (Personaje-> Personaje) }
data Personaje = UnPersonaje { 
	nombre :: String,
	salud :: Float,
	elementos :: [Elemento],
	anioPresente :: Int }

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
jack = UnPersonaje "Jack" 300 [concentracion 3, katana] 200
 where 
	katana = UnElemento "Magia"  (causarDanio 1000) id

--d.
aku :: Int -> Float -> Personaje
aku anioAku saludAku = UnPersonaje "Aku" saludAku ((concentracion 4) : (esbirrosMalvados (anioAku * 100)) : portalAFuturo anioAku)
 where
	portalAFuturo anioAku = UnElemento "Magia" (mandarAlAnio anioFuturo) (aku anioFuturo salud)
	anioFuturo = anioAku + 2800

--auxiliares

concentrar :: Int -> Personaje -> Personaje
concentrar 1 personaje = meditar personaje
concentrar x personaje = concentrar (x-1) (meditar personaje)

--------------
-- Punto 04 --
--------------

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
	| salud atacante == 0 = (defensor, atacante)
	| otherwise = luchar defensorTrasRonda atacanteTrasRonda
	where
		defensorTrasRonda = snd (todosLosElementos (elementos atacante) (atacante, defensor))
		atacanteTrasRonda = fst (todosLosElementos (elementos atacante) (atacante, defensor))

--auxiliar

aplicarElemento :: Elemento -> (Personaje , Personaje) -> (Personaje , Personaje)
aplicarElemento elemento (atacante , defensor) = ((defensa elemento) atacante, (ataque elemento) defensor)

todosLosElementos :: [Elemento] -> (Personaje , Personaje) -> (Personaje , Personaje)
todosLosElementos elementos (atacante, defensor) =
	foldl (aplicarElemento) (atacante, defensor) elementos
