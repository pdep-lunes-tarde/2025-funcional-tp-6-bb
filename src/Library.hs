module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    precioFinal :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

--Funcion Auxiliar:
calcularPrecio :: Hamburguesa -> Number
calcularPrecio hamburguesa = precioBase hamburguesa + sum (map precioIngrediente (ingredientes hamburguesa))



--Ejemplo:
cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa {
    precioBase = 20,
    precioFinal = calcularPrecio (Hamburguesa 20 0 [Pan, Carne, Cheddar, Pan]),
    ingredientes = [Pan, Carne, Cheddar, Pan]
}

agrandar :: Hamburguesa -> Hamburguesa
agrandar (Hamburguesa precioBase precioFinal ingredientes)
    | elem Carne ingredientes = Hamburguesa precioBase (precioFinal + precioIngrediente Carne) (Carne : ingredientes)
    | elem Pollo ingredientes = Hamburguesa precioBase (precioFinal + precioIngrediente Carne) (Pollo : ingredientes)
    | otherwise = Hamburguesa precioBase precioFinal ingredientes

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa {precioFinal = calcularPrecio hamburguesa,
    ingredientes = ingrediente : ingredientes hamburguesa}

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento descuento hamburguesa = hamburguesa {precioBase = precioBase hamburguesa * (1 - descuento/100),
    precioFinal = calcularPrecio hamburguesa}

pdepBurger :: Hamburguesa -> Hamburguesa
pdepBurger hamburguesa = (modificarPrecio 110 . descuento 20 . agregarIngrediente Panceta . agregarIngrediente Cheddar . agrandar . agrandar) cuartoDeLibra

dobleCuarto :: Hamburguesa -> Hamburguesa
dobleCuarto hamburguesa = (calcularPrecio . agregarIngrediente Carne . agregarIngrediente Cheddar) cuartoDeLibra

bigPdep ::  Hamburguesa -> Hamburguesa
bigPdep hamburguesa = (agregarIngrediente Curry) 

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa

--Función para modificar los precios de las hamburgesas cuyo precio final es distinto al indicado por la consigna:
modificarPrecio :: Number -> Hamburguesa -> Hamburguesa
modificarPrecio precio hamburguesa = hamburguesa {precioFinal = precio}
