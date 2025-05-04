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
        superPoder :: Poder,
        poderActivo :: Bool,
        cantidadDeVida :: Int
} deriving Show

espina :: Personaje
espina = UnPersonaje {
        nombre = "Espina",
        poderBasico = bolaEspinosa,
        superPoder = (granadaDeEspinas 5),
        poderActivo = True,
        cantidadDeVida = 4800
}

pamela :: Personaje
pamela = UnPersonaje {
        nombre = "Pamela",
        poderBasico = lluviaDeTuercas,
        superPoder = torretaCurativa,
        poderActivo = False,
        cantidadDeVida = 9600
}


{-bolaEspinosa: le quita 1000 puntos de vida a quien sea su contrincante (¡no debe quedar un número negativo!)-}

bolaEspinosa :: Poder
bolaEspinosa personaje = personaje {cantidadDeVida = max 0 (cantidadDeVida personaje -1000)}

{-granadaDeEspinas: el daño va a depender del radio de explosión de la misma. Si es mayor a 3, le agregara a su 
nombre “Espina estuvo aquí”. Si además su contrincante tiene menos de 800 vida, desactiva su súper y lo deja con 0 
de vida. En otro caso, se usa una bola de espinas.-}

granadaDeEspinas :: Int -> Poder
granadaDeEspinas radio personaje 
        | (radio > 3) && (cantidadDeVida personaje < 800) = personaje {nombre = nombre personaje ++ " Espina estuvo aqui", poderActivo = False, cantidadDeVida = 0}
        | radio > 3 =  personaje {nombre = nombre personaje ++ " Espina estuvo aqui"}
        | otherwise = bolaEspinosa personaje

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
        
{-torretaCurativa: le activa el súper a su aliado y lo deja con el doble de su salud inicial.-}

torretaCurativa :: Poder
torretaCurativa personaje 
        | esAliado personaje = personaje {poderActivo = True, cantidadDeVida = cantidadDeVida personaje * 2}
        | otherwise = personaje

{-atacar con el poder especial: si el personaje tiene el súper poder activo, entonces va a atacar 
a su contrincante con el súper y con el básico. Si no, no hará nada.-}

atacarConElPoderEspecial :: Personaje -> Personaje -> Personaje
atacarConElPoderEspecial personaje contrincante 
        | poderActivo personaje = (poderBasico personaje . superPoder personaje) contrincante
        | otherwise = contrincante