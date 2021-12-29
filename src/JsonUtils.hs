{-# LANGUAGE OverloadedStrings #-}
module JsonUtils where

import Data.Aeson ( eitherDecode )
import Data.Aeson.Types ( parseEither, (.:) )
import Data.ByteString.Lazy.Char8 as Char8Lazy ( ByteString )

extractMessage :: ByteString -> Either String (String, String)
extractMessage input = do
  object <- eitherDecode input
  let parser = (\obj -> do
                key <- obj .: "key"
                msg <- obj .: "message"
                return (key, msg))
  parseEither parser object