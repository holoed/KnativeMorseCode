{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.Trans ()
import System.Console.Haskeline ()
import Data.Maybe ( fromMaybe )
import System.Environment (lookupEnv)
import Web.Scotty         (ScottyM, scotty)
import Web.Scotty.Trans ( body, json, post, middleware )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import Data.ByteString.Lazy.Char8 as Char8 ( unpack )
import Data.Text (pack)

main :: IO ()
main = do
  Prelude.putStrLn "Morse Code Service started"
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  let p = read pStr :: Int
  scotty p route

route :: ScottyM()
route = do
    middleware logStdout
    post "/echo" $ do
         input <- body
         let ret = Char8.unpack input
         json $ pack ret

