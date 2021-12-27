module MorseCodeTests where

import Test.Hspec ( describe, it, shouldBe, SpecWith )

tests :: SpecWith ()
tests =
  describe "Morse Code Tests" $ do

     it "Test" $
       True `shouldBe` True

