{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.Trans ()
import System.Console.Haskeline ()
import Data.Maybe ( fromMaybe )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.Environment (lookupEnv)
import Web.Scotty         (ScottyM, scotty)
import Web.Scotty.Trans ( body, json, post, middleware, setHeader )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import Data.ByteString.Lazy.Char8 as Char8 ( unpack )
import Data.Text as Text
import Data.Text.Lazy as Lazy
import qualified Data.UUID.V1 as U1

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
         (Just k) <- liftIO U1.nextUUID 
         setHeader "Ce-Id" (Lazy.pack $ show k)
         setHeader "Ce-Specversion" "1.0"
         setHeader "Ce-Source" "/morse-code"
         setHeader "Ce-Type" "morseCode"
         json $ Text.pack ("{ \"data\":" ++ ret ++ "}")

