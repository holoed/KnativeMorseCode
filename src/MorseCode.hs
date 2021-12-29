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
import Network.HTTP.Types ( status200 )
import Utils (mapTuple)
import JsonUtils (extractMessage)
import MorseCodeTable (decode)
import RedisCmd (get', set', del', evalRedis, RedisCmd)
import Control.Monad.Free ( Free )

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
  let p = read pStr :: Int
  scotty p route

updateState :: ByteString -> ByteString -> Maybe ByteString -> Free RedisCmd ()
updateState key msg (Just x) = set' key (x <> msg)
updateState key msg Nothing  = set' key msg

processSpace :: ByteString -> Maybe ByteString -> Free RedisCmd (Maybe Char)
processSpace k v = do
     _ <- del' [k]
     return $ (decode . Char8.unpack) =<< v

processDashOrDot :: ByteString -> Maybe ByteString -> ByteString -> Free RedisCmd (Maybe a)
processDashOrDot k v v' = do
     _ <- updateState k v' v
     return Nothing

process :: ByteString -> ByteString -> Free RedisCmd (Maybe Char)
process key msg = do
       v <- get' key
       if msg == " " then processSpace key v
       else processDashOrDot key v msg

route :: ScottyM()
route = do
    middleware logStdout
    post "/encode" $ do
         input <- body
         let (Right (key, msg)) = mapTuple Char8.pack <$> extractMessage input
         decoded <- liftIO $ evalRedis $ process key msg
         case decoded of
           Nothing -> do
            liftIO $ putStr "Updated state, thanks."
            status status200
           Just c -> replyEvent c

