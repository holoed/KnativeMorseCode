module Utils where

import Control.Monad  (join)
import Data.Bifunctor (bimap)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join bimap