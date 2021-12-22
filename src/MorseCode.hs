{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.Trans ()
import System.Console.Haskeline ()
import Data.Maybe ( fromMaybe )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import System.Environment (lookupEnv)
import Web.Scotty         (ScottyM, scotty)
import Web.Scotty.Trans ( body, text, post, middleware, setHeader, status )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import Data.ByteString.Lazy.Char8 as Char8Lazy ( ByteString )
import Data.ByteString.Char8 as Char8 ( pack, unpack )
import Data.Text.Lazy as Lazy ( pack )
import qualified Data.UUID.V1 as U1
import Database.Redis (Connection, ConnectInfo, connect, connectHost, defaultConnectInfo, runRedis, set, get, del)
import Data.Aeson ( eitherDecode )
import Data.Aeson.Types ( parseEither, (.:) )
import Control.Monad  (join)
import Data.Bifunctor (bimap)
import Network.HTTP.Types ( status200 )

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join bimap

extractMessage :: ByteString -> Either String (String, String)
extractMessage input = do
  object <- eitherDecode input
  let parser = (\obj -> do
                key <- obj .: "key"
                msg <- obj .: "message"
                return (key, msg))
  parseEither parser object

main :: IO ()
main = do
  Prelude.putStrLn "Morse Code Service started"
  pStr <- fromMaybe "8080" <$> lookupEnv "PORT"
  redisCon <- connect connectionInfo
  let p = read pStr :: Int
  scotty p (route redisCon)

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectHost  = "redis-master" }

decode :: [Char] -> Maybe Char
decode ".-"   = Just 'A'
decode "-..." = Just 'B'
decode "-.-." = Just 'C'
decode "-.."  = Just 'D'
decode "."    = Just 'E'
decode "..-." = Just 'F'
decode "--."  = Just 'G'
decode _ = Nothing

route :: Connection -> ScottyM()
route redisCon = do
    middleware logStdout
    post "/encode" $ do
         input <- body
         let (Right (key, msg)) = mapTuple Char8.pack <$> extractMessage input
         decoded <- liftIO $ runRedis redisCon $ do
                         v <- get key
                         liftIO $ print v
                         if msg == " " then do
                          _ <- del [key]
                          return $ decode (Char8.unpack msg)
                         else do
                          _ <- case v of
                            Right (Just x) -> set key (x <> msg)
                            Right Nothing -> set key msg
                            Left reply -> set key (Char8.pack (show reply))
                          return Nothing
         case decoded of
           Nothing -> 
             status status200
           Just c -> do
            (Just k) <- liftIO U1.nextUUID
            setHeader "Ce-Id" (Lazy.pack $ show k)
            setHeader "Ce-Specversion" "1.0"
            setHeader "Ce-Source" "/morse-code"
            setHeader "Ce-Type" "morseCode"
            text $ Lazy.pack ("{ \"data\": { \"message\": " ++ [c] ++ " } }")

