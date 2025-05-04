module Lib () where

import Text.Show.Functions ()

{-Por el momento sólo tendremos a los personajes Espina 🌵 y Pamela 👩‍⚕️, y de éstos nos 
interesa saber:
su nombre;
su poder básico;
su súper poder;
si tiene el súper poder activo y
su cantidad de vida.
-}

type Poder = Personaje -> Personaje

equipo :: [Personaje]
equipo = [espina, pamela]

data Personaje = UnPersonaje {
        nombre :: String,
        poderBasico :: Poder,
        --superPoder :: Poder,
        radioSuperPoder :: Int,
        poderActivo :: Bool,
        cantidadDeVida :: Int
} deriving Show

espina :: Personaje
espina = UnPersonaje {
        nombre = "Espina",
        poderBasico = bolaEspinosa,
        --superPoder = granadaDeEspinas,
        radioSuperPoder = 5,
        poderActivo = True,
        cantidadDeVida = 4800
}

pamela :: Personaje
pamela = UnPersonaje {
        nombre = "Pamela",
        poderBasico = lluviaDeTuercas,
        --superPoder = torretaCurativa,
        radioSuperPoder = 0,
        poderActivo = False,
        cantidadDeVida = 9600
}


{-bolaEspinosa: le quita 1000 puntos de vida a quien sea su contrincante (¡no debe quedar un número negativo!)-}

bolaEspinosa :: Poder
bolaEspinosa personaje = personaje {cantidadDeVida = max 0 (cantidadDeVida personaje -1000)}

{-granadaDeEspinas: el daño va a depender del radio de explosión de la misma. Si es mayor a 3, le agregara a su 
nombre “Espina estuvo aquí”. Si además su contrincante tiene menos de 800 vida, desactiva su súper y lo deja con 0 
de vida. En otro caso, se usa una bola de espinas.-}
{-
granadaDeEspinas :: Personaje -> Personaje
granadaDeEspinas personaje =
        | 
-}

{-lluviaDeTuercas: pueden ser sanadoras o dañinas. Las primeras le suman 800 puntos de vida a su colega y las 
segundas le disminuyen a la mitad la vida de quien sea su contrincante. En cualquier otro caso, no le pasa nada al 
personaje.
-}

esAliado :: Personaje -> Bool
esAliado personaje = elem (nombre personaje) (map nombre equipo)

lluviaDeTuercas :: Poder
lluviaDeTuercas personaje 
        | esAliado personaje = personaje {cantidadDeVida = cantidadDeVida personaje + 800}
        | otherwise = personaje {cantidadDeVida = div (cantidadDeVida personaje) 2}
        
