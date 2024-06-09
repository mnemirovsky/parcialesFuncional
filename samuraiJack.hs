import Text.Show.Functions()

data Elemento = UnElemento { tipo :: String,
				  ataque :: (Personaje-> Personaje),
				  defensa :: (Personaje-> Personaje) }
data Personaje = UnPersonaje { nombre :: String,
				    salud :: Float,
				    elementos :: [Elemento],
				    anioPresente :: Int }

--------------
-- Punto 01 --
--------------

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio unAnio unPersonaje = unPersonaje {anioPresente = unAnio}

cambiarSalud :: (Float -> Float) -> Personaje -> Personaje
cambiarSalud f unPersonaje = unPersonaje {salud = f (salud unPersonaje)}

meditar :: Personaje -> Personaje
meditar = cambiarSalud (*1.5) 

causarDanio :: Float -> Personaje -> Personaje
causarDanio danio unPersonaje 
	| salud unPersonaje - danio < 0 = cambiarSalud (const 0) unPersonaje
	| otherwise                     = cambiarSalud (- danio) unPersonaje

--------------
-- Punto 02 --
--------------

esMalvado :: Personaje -> Bool
esMalvado unPersonaje = elem "Maldad" (map tipo (elementos unPersonaje))

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje UnElemento =
	salud unPersonaje - ((ataque unElemento) unPersonaje)

enemigoMortal :: Personaje -> Personaje -> Bool
enemigoMortal luchador enemigo =
	any ((==0) . salud (((ataque (UnElemento enemigo)) luchador)))

--enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
--enemigosMortales luchador enemigos =
--	filter (enemigoMortal) 

--------------
-- Punto 03 --
--------------

concentrar :: Int -> Personaje -> Personaje
concentrar nivel unPersonaje=
	foldl ($) unPersonaje (take nivel (repeat meditar)) 

concentracion :: Int -> Elemento
concentracion nivel = UnElemento "Magia" id (concentrar nivel)

katana :: Elemento
katana = UnElemento "Magia"  (causarDanio 1000) id

jack :: Personaje
jack = UnPersonaje "Jack" 300 [concentracion 3, katana] 200