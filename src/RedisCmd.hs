{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module RedisCmd where

import Control.Monad.Free ( liftF, Free, MonadFree, iterM )
import Control.Monad.Free.TH (makeFree)
import Data.ByteString.Char8 as Char8 ( ByteString )
import Database.Redis (Redis, ConnectInfo, connect, connectHost, defaultConnectInfo, runRedis, set, get, del)
import Data.Either (fromRight)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Control.Monad.State as State
import qualified Data.Map as Map

data RedisCmd next = Get' ByteString (Maybe ByteString -> next)
                   | Set' ByteString ByteString next
                   | Del' [ByteString] next
                   deriving (Functor)

type RedisCmdM = Free RedisCmd

makeFree ''RedisCmd

redisInterpreter :: Free RedisCmd a -> Redis a
redisInterpreter = iterM run
   where
     run (Get' k n) = n . fromRight Nothing =<< get k
     run (Set' k v n) = set k v >> n
     run (Del' k n) = del k >> n

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectHost  = "redis-master" }

evalRedis :: Free RedisCmd a -> IO a
evalRedis code = 
    do redisCon <- connect connectionInfo
       liftIO $ runRedis redisCon (redisInterpreter code)

mockInterpreter :: Free RedisCmd a -> State.State (Map.Map ByteString ByteString) a
mockInterpreter = iterM run
   where
     run (Get' k n) = do dict <- State.get
                         n $ Map.lookup k dict 
     run (Set' k v n) = do dict <- State.get
                           let dict' = Map.insert k v dict 
                           State.put dict'
                           n  
     run (Del' [k] n) = do dict <- State.get 
                           let dict' = Map.delete k dict
                           State.put dict'
                           n 
     run (Del' _ n) = n

evalMock :: Free RedisCmd a -> IO a
evalMock code = 
    return $ State.evalState (mockInterpreter code) Map.empty

