module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "Parte 1" $ do
        describe "Funcion agrandar:" $ do
            it "agrandar debería agregar una unidad de Carne a la lista de ingredientes de la hamburguesa." $ do
                agrandar cuartoDeLibra `shouldBe` Hamburguesa 20 74 [Carne, Pan, Carne, Cheddar, Pan]
            it "agrandar debería agregar una unidad de Pollo a la lista de ingredientes de la hamburguesa." $ do
                agrandar (Hamburguesa 10 34 [Pollo, Pan, Pan, Cheddar]) `shouldBe` Hamburguesa 10 44 [Pollo, Pollo, Pan, Pan, Cheddar]
            it "agrandar debería agregar una unidad de PatiVegano a la lista de ingredientes de la hamburguesa." $ do
                agrandar (Hamburguesa 10 34 [PatiVegano, Pan, Pan, Cheddar]) `shouldBe` Hamburguesa 10 44 [PatiVegano, PatiVegano, Pan, Pan, Cheddar] 
        describe "Funcion agregarIngrediente:" $ do
            it "agregarIngrediente debería agregar a la lista de ingredientes el ingrediente deseado y actualizar el precioFinal." $ do
                agregarIngrediente Cheddar cuartoDeLibra `shouldBe` Hamburguesa 20 64 [Cheddar, Pan, Carne, Cheddar, Pan]
        describe "Funcion descuento:" $ do
            it "descuento deberia aplicar el descuento deseado al precio base de la hamburguesa." $ do
                descuento 10  cuartoDeLibra `shouldBe` Hamburguesa 18 52 [Pan, Carne, Cheddar, Pan]
        describe "pdepBurger:" $ do
            it "pdepBuruger debería ser un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento. El precio deberia ser 110." $ do
                pdepBurger `shouldBe` Hamburguesa 16 110 [Panceta, Cheddar, Carne, Carne, Pan, Carne, Cheddar, Pan]


    describe "Parte 2" $ do
        describe "dobleCuarto:" $ do
            it "dobleCuarto debería ser un cuarto de libra con Carne y cheddar. El precio deberia ser 84." $ do
                dobleCuarto `shouldBe` Hamburguesa 20 84 [Carne, Cheddar, Pan, Carne, Cheddar, Pan]
        describe "bigPdep:" $ do
            it "bigPdep debería ser un doble cuarto con curry. El precio final deberia ser 89." $ do
                bigPdep `shouldBe` Hamburguesa 20 89 [Curry, Carne, Cheddar, Pan, Carne, Cheddar, Pan]
        describe "delDia:" $ do
            it "delDia debería recibir una hamburguesa, agregarle papas y aplicar un 30% de descuento. El precio final deberia ser 88." $ do
                delDia bigPdep `shouldBe` Hamburguesa 14 93 [Papas, Curry, Carne, Cheddar, Pan, Carne, Cheddar, Pan]


    describe "Parte 3" $ do
        describe "Funcion hacerVeggie:" $ do
            it "hacerVeggie cambia la Carne y el Pollo que hayan en la hamburguesa por PatiVegano, el cheddar lo cambia por queso de almendras y la panceta por bacon de tofu. Actualiza el precioFinal." $ do
                hacerVeggie (Hamburguesa 20 64 [Pan, Carne, Panceta, Cheddar, Pan]) `shouldBe` Hamburguesa 20 61 [Pan, PatiVegano, BaconDeTofu, QuesoDeAlmendras, Pan]
        describe "Funcion cambiarPanDePati:" $ do
            it "cambiarPanDePati cambia el Pan que haya en la hamburguesa por PanIntegral. Actualiza el precio." $ do
                cambiarPanDePati cuartoDeLibra `shouldBe` Hamburguesa 20 56 [PanIntegral, Carne, Cheddar, PanIntegral]
        describe "dobleCuartoVegano:" $ do
            it "dobleCuartoVegano deberia ser un dobleCuarto veggie con pan integral." $ do
                dobleCuartoVegano `shouldBe` Hamburguesa 20 76 [PatiVegano, QuesoDeAlmendras, PanIntegral, PatiVegano, QuesoDeAlmendras, PanIntegral]