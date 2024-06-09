-------------
-- Punto 1 --
-------------

data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgaste :: Desgaste,
    velocidadMax :: Float,
    tiempo :: Float
} deriving (Show, Eq)

type Desgaste = (Float, Float)

ferrari = Auto "Ferrari" "F50" (0,0) 65 0
lamborghini = Auto "Lamborghini" "Diablo" (7,4) 73 0
fiat = Auto "Fiat" "600" (33,27) 44 0

-------------
-- Punto 2 --
-------------

chasis :: Auto -> Float
chasis = fst . desgaste

ruedas :: Auto -> Float
ruedas = snd . desgaste

buenEstado :: Auto -> Bool
buenEstado unAuto = ruedas unAuto < 40 && chasis unAuto < 60

noDaMas :: Auto -> Bool
noDaMas unAuto = chasis unAuto > 80 || ruedas unAuto > 80

-------------
-- Punto 3 --
-------------

cambiarDesgasteChasis ::  (Float -> Float) -> Auto -> Auto
cambiarDesgasteChasis funcion unAuto = unAuto {desgaste = ((funcion . chasis) unAuto, ruedas unAuto)}


cambiarDesgasteRuedas ::  (Float -> Float) -> Auto -> Auto
cambiarDesgasteRuedas funcion unAuto = unAuto{ desgaste = (chasis unAuto, (funcion . ruedas) unAuto)}

repararUnAuto :: Auto -> Auto
repararUnAuto unAuto = (cambiarDesgasteChasis (*0.15)) . (cambiarDesgasteRuedas (const 0)) $ unAuto


-------------
-- Punto 4 --
-------------

type Tramo = Auto -> Auto


agregarTiempo :: Float -> Auto -> Auto
agregarTiempo segundos unAuto = unAuto { tiempo = tiempo unAuto + segundos}


curva :: Float -> Float -> Tramo
curva angulo longitud unAuto = desgasteCurva . (agregarTiempo segundos) $ unAuto
 where
    desgasteCurva = cambiarDesgasteRuedas (+(3 * longitud / angulo))
    segundos = (velocidadMax unAuto) / 2

curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

curvaTranca :: Tramo
curvaTranca = curva 110 550

tramoRecto :: Float -> Tramo
tramoRecto longitud unAuto = desgasteRecta . (agregarTiempo segundos) $ unAuto
 where
    desgasteRecta = (cambiarDesgasteChasis (+(0.1*longitud)))
    segundos = longitud / (velocidadMax unAuto) 

tramoRectoClassic :: Tramo
tramoRectoClassic = tramoRecto 750

tramito :: Tramo
tramito = tramoRecto 280

boxes :: Tramo -> Tramo
boxes unTramo unAuto
    | buenEstado unAuto =  unAuto
    | otherwise = agregarTiempo 10 . repararUnAuto $ unAuto

mojar :: Tramo -> Tramo
mojar unTramo unAuto = agregarTiempo segundosPorMojado . unTramo $ unAuto
 where
    segundosPorMojado = (tiempo (unTramo unAuto) - tiempo unAuto) / 2

 
ripiar :: Tramo -> Tramo
ripiar unTramo = unTramo . unTramo


tramoObstruido :: Float -> Tramo -> Tramo
tramoObstruido pistaOcupada unTramo unAuto = 
    cambiarDesgasteRuedas (+ (2 * pistaOcupada)) (unTramo unAuto) 


-------------
-- Punto 5 --
-------------

type Pista = [Tramo]

pasarPorTramo :: Auto -> Tramo -> Auto
pasarPorTramo unAuto unTramo
    | noDaMas unAuto = unAuto
    | otherwise      = unTramo unAuto

-------------
-- Punto 6 --
-------------


superPista :: [Tramo]
superPista = [
    tramoRectoClassic,
    curvaTranca,
    (tramito . mojar tramito),  
    tramoObstruido 2 (curva 80 400),
    curva 115 650,
    tramoRecto 970,
    curvaPeligrosa,
    ripiar tramito,
    boxes (tramoRecto 800)
    ]

-------------
-- Punto 7 --
-------------

data Carrera = Carrera {
    pista   :: Pista,
    vueltas :: Int
}

tourBuenosAires = Carrera superPista 20

listaDeAutos :: [Auto]
listaDeAutos = [ferrari, lamborghini, fiat]

--c--

correrVuelta :: Pista -> Auto -> Auto
correrVuelta unaPista unAuto = foldl pasarPorTramo unAuto unaPista

peganUnaVuelta :: Pista -> [Auto] -> [Auto]
peganUnaVuelta unaPista = (filter (not . noDaMas)) . map (correrVuelta unaPista)


correr :: [Auto] -> Carrera -> [[Auto]]
correr autos unaCarrera = 
    take (vueltas unaCarrera) . iterate (peganUnaVuelta (pista unaCarrera)) $ autos