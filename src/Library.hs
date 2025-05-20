module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

precisionDelJugador = precisionJugador . habilidad
fuerzaDelJugador = fuerzaJugador . habilidad

between n m x = elem x [n .. m]

tiroPiola = UnTiro 100 100 0

tiroFallido = UnTiro 0 0 0

type Palo = Jugador -> Tiro

putter :: Palo
putter jugador = UnTiro {
  velocidad = 10,
  precision = ((*2) . precisionDelJugador) jugador ,
  altura = 0
}

madera :: Palo
madera jugador = UnTiro {
  velocidad = 100,
  precision = ((`div` 2) . precisionDelJugador) jugador ,
  altura = 5
}

hierro :: Number -> Palo 
hierro n jugador = UnTiro {
  velocidad = ((*n) . fuerzaDelJugador) jugador,
  precision = ((`div` n) . precisionDelJugador) jugador ,
  altura = max 0 (n-3)
}

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo jugador

type Obstaculo = Tiro -> Tiro


obstaculo :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Tiro -> Tiro
obstaculo criterioSuperacion modificadorTiro tiro 
  | criterioSuperacion tiro = modificadorTiro tiro
  | otherwise = tiroFallido

-- Opcion 1 --
tunelConRampita :: Obstaculo
tunelConRampita tiro =
        obstaculo supera modificador tiro
        where
          supera tiro = precision tiro > 90 && altura tiro == 0
          modificador tiro = UnTiro {
            velocidad = ((*2) . velocidad ) tiro,
            precision = 100,
            altura = 0
          }

-- Opcion 2 --
-- tunelConRampita :: Tiro -> Tiro
-- tunelConRampita tiro 
--         | precision tiro > 90 && altura tiro == 0 =
--           UnTiro {
--             velocidad = ((*2) . velocidad ) tiro,
--             precision = 100,
--             altura = 0
--           }
--         | otherwise = tiroFallido

laguna :: Number -> Obstaculo
laguna largo tiro
        | velocidad tiro > 80 && between 1 5 (altura tiro) =
            tiro {
              altura = ((`div` largo) . altura) tiro
            }
        | otherwise = tiroFallido

hoyo :: Obstaculo
hoyo tiro
        | between 5 20 (velocidad tiro) && altura tiro == 0 && precision tiro > 95 =
            UnTiro 0 0 0
        | otherwise = tiroFallido


obstaculos = [hoyo, tunelConRampita, laguna 5]

palosUtiles jugador obstaculo =
      filter (\palo -> obstaculo (golpe jugador palo /= tiroFallido)) palos

-- maximoSegun f = foldl1 (mayorSegun f)
-- mayorSegun f a b
--   | f a > f b = a
--   | otherwise = b

