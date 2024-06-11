--- PARCIAL GRAVITY FALLS ---

--Primera Parte:

--------------
-- Punto 01 --
--------------

data Personaje = Personaje {
    edad        :: Int,
    items       :: [String],
    experiencia :: Int
}

data Criatura = Criatura {
    peligrosidad :: Int,
    debilidad    :: Debilidad
}

type Debilidad = Personaje -> Bool

siempredetras :: Criatura
siempredetras = Criatura 0 (invencible)

gnomos        :: Int -> Criatura
gnomos cantidad = Criatura (2 ^ cantidad) (tieneEnInventario "Soplador de Hojas")

fantasma     :: Int -> Debilidad -> Criatura
fantasma categoria asuntoNoResuelto =
    Criatura (categoria * 20) (asuntoNoResuelto)

--Auxiliares
invencible :: Debilidad
invencible unPersonaje = False

tieneEnInventario :: String -> Debilidad
tieneEnInventario objeto  = (any ((== objeto))) . items 

--------------
-- Punto 02 --
--------------

enfrentarCriatura :: Personaje -> Criatura -> Personaje
enfrentarCriatura unPersonaje unaCriatura 
    | (debilidad unaCriatura) unPersonaje = ganarExperiencia (peligrosidad unaCriatura) unPersonaje 
    | otherwise = ganarExperiencia 1 unPersonaje

--Auxiliares

ganarExperiencia :: Int -> Personaje -> Personaje
ganarExperiencia xpNueva unPersonaje = unPersonaje { experiencia = experiencia unPersonaje + xpNueva}

--------------
-- Punto 03 --
--------------

--a. 
experienciaGanada :: [Criatura] -> Personaje -> Int
experienciaGanada unasCriaturas unPersonaje = 
    experiencia (enfrentarCriaturas unasCriaturas unPersonaje) - experiencia unPersonaje

--auxiliares
enfrentarCriaturas :: [Criatura] -> Personaje -> Personaje
enfrentarCriaturas unasCriaturas unPersonaje =
    foldl (enfrentarCriatura) unPersonaje unasCriaturas

--b.
grupitoDeMonstruos :: [Criatura]
grupitoDeMonstruos = [
    siempredetras, 
    gnomos 10, 
    fantasma 3 (ninioDisfrazadoDeOveja), 
    fantasma 1 ((>10) . experiencia)]

--auxiliares 
ninioDisfrazadoDeOveja :: Debilidad
ninioDisfrazadoDeOveja unPersonaje = (((< 13) . edad) $ unPersonaje) && (tieneEnInventario "disfraz de oveja" unPersonaje)



-- Segunda Parte:

--------------
-- Punto 01 --
--------------

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ _ [] = []
zipWithIf funcion condicion (x:xs) (y:ys)
    | condicion y = 
        (funcion x y : zipWithIf funcion condicion (xs) (ys))
    | otherwise   = 
        (y : zipWithIf funcion condicion (x:xs) (ys))

--------------
-- Punto 02 --
--------------

--a.
abecedarioDesde :: Char -> [Char]
abecedarioDesde 'a'   = ['a'..'z']
abecedarioDesde letra = [letra..'z'] ++ ['a'..letra]

--b.
desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraClave letraEncriptada =
  fst . head . (filter ((== letraEncriptada) . snd)) . paresDeLetras $ letraClave

--auxiliar
paresDeLetras ::  Char -> [(Char, Char)]
paresDeLetras letraClave = zip (abecedarioDesde 'a') (abecedarioDesde letraClave)

--c. 
cesar :: String -> Char -> String
cesar texto letraClave = zipWithIf (desencriptarLetra) (($ (abecedarioDesde 'a')) . elem ) (repeat letraClave) texto

--d. 
todosLosCesar ::String -> [String]
todosLosCesar texto = map (cesar texto) (abecedarioDesde 'a')

--------------
-- Punto 03 --
--------------

vigenere :: String -> String -> String
vigenere palabraClave texto = zipWithIf (desencriptarLetra) (($ (abecedarioDesde 'a')) . elem ) (cycle palabraClave) texto