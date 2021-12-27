module Main where

import Test.Hspec ( hspec )
import qualified MorseCodeTests

main :: IO ()
main = hspec $ do
    MorseCodeTests.tests
 