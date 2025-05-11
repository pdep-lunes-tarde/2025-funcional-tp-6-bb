module Library where
import PdePreludat
import GHC.IO.Handle.Types (Handle__(haBufferMode))

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | BaconDeTofu
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12 
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    precioFinal :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

--Funcion Auxiliar:
calcularPrecio :: Hamburguesa -> Number
calcularPrecio hamburguesa = sum (map precioIngrediente (ingredientes hamburguesa))

actualizarPrecioFinal :: Hamburguesa -> Hamburguesa
actualizarPrecio hamburguesa = hamburguesa { precioFinal = sum (map precioIngrediente (ingredientes hamburguesa)) }

--Función para modificar los precios de las hamburgesas cuyo precio final es distinto al indicado por la consigna:
modificarPrecio :: Number -> Hamburguesa -> Hamburguesa
modificarPrecio precio hamburguesa = hamburguesa {precioFinal = precio}

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
    | elem PatiVegano ingredientes = Hamburguesa precioBase (precioFinal + precioIngrediente PatiVegano) (PatiVegano : ingredientes)
    | otherwise = Hamburguesa precioBase precioFinal ingredientes

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa =
  hamburguesa { 
    ingredientes = ingrediente : ingredientes hamburguesa,
    precioFinal = calcularPrecio (hamburguesa { ingredientes = ingrediente : ingredientes hamburguesa })
  }

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento descuento hamburguesa =
  actualizarPrecio $ hamburguesa { 
    precioBase = precioBase hamburguesa * (1 - descuento / 100) }

pdepBurger :: Hamburguesa
pdepBurger = (modificarPrecio 110 . descuento 20 . agregarIngrediente Panceta . agregarIngrediente Cheddar . agrandar . agrandar) cuartoDeLibra

dobleCuarto :: Hamburguesa
dobleCuarto = (actualizarPrecio . agregarIngrediente Carne . agregarIngrediente Cheddar) cuartoDeLibra

bigPdep ::  Hamburguesa
bigPdep = (actualizarPrecio . agregarIngrediente Curry) dobleCuarto

delDia :: Hamburguesa
delDia = (agregarIngrediente Papas . descuento 30) bigPdep

cambiarIngredienteVegano :: Ingrediente -> Ingrediente
cambiarIngrediente Carne = PatiVegano
cambiarIngrediente Pollo = PatiVegano
cambiarIngrediente Cheddar = QuesoDeAlmendras
cambiarIngrediente Panceta = BaconDeTofu
cambiarIngrediente ingrediente = ingrediente --Si el ingrediente no es uno de los anteriores, lo dejamos igual

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = hamburguesa { 
    ingredientes = map cambiarIngredienteVegano (ingredientes hamburguesa), 
    precioFinal = calcularPrecio (hamburguesa { ingredientes = map cambiarIngredienteVegano (ingredientes hamburguesa) }) 
}

cambiarIngredientePan :: Ingrediente -> Ingrediente
cambiarIngredientePan Pan = PanIntegral

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = hamburguesa {
    ingredientes = map cambiarIngredientePan (ingredientes hamburguesa),
    precioFinal = calcularPrecio (hamburguesa { ingredientes = map cambiarIngredientePan (ingredientes hamburguesa) }) 
}

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = (actualizarPrecio . hacerVeggie . cambiarPanDePati . agregarIngrediente Cheddar) dobleCuarto