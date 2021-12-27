{-# LANGUAGE OverloadedStrings #-}

module MorseCode where

import Control.Monad.Trans ()
import System.Console.Haskeline ()
import Data.Maybe ( fromMaybe )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.Environment (lookupEnv)
import Web.Scotty         (ScottyM, scotty)
import Web.Scotty.Trans ( body, raw, post, middleware, setHeader, status, ActionT )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import Data.ByteString.Lazy.Char8 as Char8Lazy ( pack )
import Data.ByteString.Char8 as Char8 ( pack, unpack, ByteString )
import Data.Text.Lazy as Lazy ( Text, pack )
import qualified Data.UUID.V1 as U1
import Database.Redis (Reply, Redis, Connection, ConnectInfo, Status, connect, connectHost, defaultConnectInfo, runRedis, set, get, del)
import Network.HTTP.Types ( status200 )
import Utils (mapTuple)
import JsonUtils (extractMessage)
import MorseCodeTable (decode)

replyEvent :: Char -> ActionT Text IO ()
replyEvent c = do
            (Just k) <- liftIO U1.nextUUID
            setHeader "Ce-Id" (Lazy.pack $ show k)
            setHeader "Ce-Specversion" "1.0"
            setHeader "Ce-Source" "morse-code-decoded"
            setHeader "Ce-Type" "morse-Code"
            setHeader "Content-Type" "application/json"
            liftIO $ print ("Decoded letter: " ++ [c])
            raw $ Char8Lazy.pack ("{ \"data\": { \"message\": \"" ++ [c] ++ "\" } }")

main :: IO ()
main = do
  Prelude.putStrLn "Morse Code Service started"
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  redisCon <- connect connectionInfo
  let p = read pStr :: Int
  scotty p (route redisCon)

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectHost  = "redis-master" }

updateState :: ByteString -> ByteString -> Either Reply (Maybe ByteString)-> Redis (Either Reply Status)
updateState key msg (Right (Just x)) = set key (x <> msg)
updateState key msg (Right Nothing)  = set key msg
updateState key _   (Left reply)     = set key (Char8.pack (show reply))

processSpace :: ByteString -> Either Reply (Maybe ByteString) -> Redis (Maybe Char)
processSpace k v = do             
     _ <- del [k]
     let Right (Just x) = v
     return $ decode (Char8.unpack x)

processDashOrDot :: ByteString -> Either Reply (Maybe ByteString) -> ByteString -> Redis (Maybe a)
processDashOrDot k v v' = do
     _ <- updateState k v' v
     return Nothing

process :: ByteString -> ByteString -> Redis (Maybe Char)
process key msg = do
       v <- get key
       liftIO $ print v
       if msg == " " then processSpace key v
       else processDashOrDot key v msg

route :: Connection -> ScottyM()
route redisCon = do
    middleware logStdout
    post "/encode" $ do
         input <- body
         let (Right (key, msg)) = mapTuple Char8.pack <$> extractMessage input
         decoded <- liftIO $ runRedis redisCon $ process key msg
         case decoded of
           Nothing -> do
            liftIO $ putStr "Updated state, thanks."
            status status200
           Just c -> replyEvent c 

