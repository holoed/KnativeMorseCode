{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module RedisCmd where

import Control.Monad.Free ( liftF, Free, MonadFree )
import Control.Monad.Free.TH (makeFree)

data RedisCmd next = Get' String (Maybe String -> next) 
                   | Set' String String next      
                   deriving (Functor)

type RedisCmdM = Free RedisCmd

makeFree ''RedisCmd