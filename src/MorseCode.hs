{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.Trans ()
import System.Console.Haskeline ()
import Data.Maybe ( fromMaybe )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.Environment (lookupEnv)
import Web.Scotty         (ScottyM, scotty)
import Web.Scotty.Trans ( body, text, post, middleware, setHeader )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import Data.ByteString.Lazy.Char8 as Char8Lazy ( unpack )
import Data.ByteString.Char8 as Char8 ( pack )
import Data.Text.Lazy as Lazy ( pack )
import qualified Data.UUID.V1 as U1
import Database.Redis (Connection, ConnectInfo, connect, connectHost, defaultConnectInfo, runRedis, set, get)

main :: IO ()
main = do
  Prelude.putStrLn "Morse Code Service started"
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  redisCon <- connect connectionInfo
  let p = read pStr :: Int
  scotty p (route redisCon) 

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectHost  = "redis-master" }

route :: Connection -> ScottyM()
route redisCon = do
    middleware logStdout
    post "/echo" $ do
         input <- body
         let ret = Char8Lazy.unpack input
         _ <- liftIO $ runRedis redisCon $ do
                         Right (Just x) <- get (Char8.pack ret)
                         set "hello" x
         (Just k) <- liftIO U1.nextUUID 
         setHeader "Ce-Id" (Lazy.pack $ show k)
         setHeader "Ce-Specversion" "1.0"
         setHeader "Ce-Source" "/morse-code"
         setHeader "Ce-Type" "morseCode"
         text $ Lazy.pack ("{ \"data\":" ++ ret ++ "}")

