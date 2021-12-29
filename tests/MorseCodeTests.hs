{-# LANGUAGE OverloadedStrings #-}
module MorseCodeTests where

import MorseCode (process)
import RedisCmd (RedisCmd, evalMock)
import Test.Hspec ( describe, it, shouldBe, SpecWith )
import Data.ByteString.Char8 as Char8 ( ByteString, singleton )
import Control.Monad.Free (Free)

decodeChar :: ByteString -> [Char] -> Free RedisCmd (Maybe Char)
decodeChar k vs = last <$> sequence (process k . singleton <$> vs)
              
tests :: SpecWith ()
tests =
  describe "Morse Code Tests" $ do

    it "decode ' '" $ do
       v1 <- evalMock $ process "42" " "
       v1 `shouldBe` Nothing

    it "decode A" $ do
       v1 <- evalMock $ do _ <- process "42" "."
                           _ <- process "42" "-"
                           process "42" " "
       v1 `shouldBe` Just 'A'

    it "decode K" $ do
       v1 <- evalMock $ do _ <- process "42" "-"
                           _ <- process "42" "."
                           _ <- process "42" "-"
                           process "42" " "
       v1 `shouldBe` Just 'K'

    it "decode KA" $ do
       v1 <- evalMock $ do x <- decodeChar "42" "-.- "
                           y <- decodeChar "42" ".- "
                           return (x, y)
       v1 `shouldBe` (Just 'K', Just 'A')


