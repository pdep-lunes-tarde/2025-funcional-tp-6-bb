module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
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
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

--Ejemplo:
cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa {
    precioBase = 20,
    ingredientes = [Pan, Carne, Cheddar, Pan]
}

agrandar :: Hamburguesa -> Hamburguesa
agrandar (Hamburguesa precioBase ingredientes)
    | any (== Carne) ingredientes = Hamburguesa (precioBase + precioIngrediente Carne) (Carne : ingredientes)
    | any (== Pollo) ingredientes = Hamburguesa (precioBase + precioIngrediente Pollo) (Pollo : ingredientes)
    | otherwise = Hamburguesa precioBase ingredientes
