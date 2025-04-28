module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

{-Por el momento sólo tendremos a los personajes Espina y Pamela, y de éstos nos interesa saber:
su nombre;
su poder básico;
su súper poder;
si tiene el súper poder activo y
su cantidad de vida.

Los poderes son los siguientes:
bolaEspinosa: le quita 1000 puntos de vida a quien sea su contrincante (¡no debe quedar un número negativo!).

lluviaDeTuercas: pueden ser sanadoras o dañinas. Las primeras le suman 800 puntos de vida a su colega y las segundas 
le disminuyen a la mitad la vida de quien sea su contrincante. En cualquier otro caso, no le pasa nada al personaje.

granadaDeEspinas: el daño va a depender del radio de explosión de la misma. Si es mayor a 3, le agregara a su nombre 
“Espina estuvo aquí”. Si además su contrincante tiene menos de 800 vida, desactiva su súper y lo deja con 0 de vida. 
En otro caso, se usa una bola de espinas.

torretaCurativa: le activa el súper a su aliado y lo deja con el doble de su salud inicial.

Además se quiere reportar lo siguiente:

atacar con el poder especial: si el personaje tiene el súper poder activo, entonces va a atacar a su contrincante con 
el súper y con el básico. Si no, no hará nada.
saber quiénes están en las últimas: es decir, el nombre de aquellos brawlers que tienen menos de 800 puntos de vida.

Modelar los poderes.
Modelar los reportes.
Modelar a Espina con 4800 puntos de vida, cuyo básico es la bola de espinas y su súper la granada de espinas 
de 5 metros de radio. ¡Siempre tiene el súper activo!
Modelar a Pamela con 9600 puntos de vida, cuyo básico es la lluvia de tuercas sanadoras y el súper la torreta curativa (full soporte). 
No tiene el súper activo

-}

{- type Poder -}
data Personaje = UnPersonaje {
        nombre :: String,
        poderBasico :: String,
        superPoder :: String,
        estadoDelPoder :: Bool,
        cantidadDeVida :: Int
} deriving Show

espina :: Personaje
espina = UnPersonaje "Espina" "Bola de Espinas" "Granada de Espinas" True 4800

pamela :: Personaje
pamela = UnPersonaje "Pamela" "Lluvia De Tuercas Sanadoras" "La Torreta Curativa" False 9600

{-bolaEspinosa: le quita 1000 puntos de vida a quien sea su contrincante (¡no debe quedar un número negativo!).-}

{-( Hacer funcion que reciba una cantidad de puntos y me diga si el personaje queda con vida <0 )-}

{-tieneVida :: Int -> Personaje -> Bool
tieneVida unaCantidad personaje = (cantidadDeVida personaje - unaCantidad) > 0 -}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = unPersonaje {cantidadDeVida = max 0 (cantidadDeVida unPersonaje -1000)}


