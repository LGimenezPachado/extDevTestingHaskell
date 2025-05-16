module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "f" $ do
    it "devuelve la paridad de los números del 1 al 4" $ do
      f `shouldBe` [False, True, False, True]