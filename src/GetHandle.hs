{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module GetHandle (handleHandler) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString         as B
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Network.HTTP.Simple
import           Servant
import           Text.Hex
import           Types

newtype RawHandleAddr = Address { getAddr :: TL.Text }
  deriving (Show)

instance FromJSON RawHandleAddr where
  parseJSON = withObject "RawhandleAddr" $ fmap Address . (.: "address") <=< (.: "tx_out")

query :: TL.Text -> Query
query hexHandleName =
  [ ("select", Just "id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)"),
    ("order", Just "id.desc"),
    ("limit", Just "1"),
    ("multi_asset.policy", Just "eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a"),
    ("multi_asset.name", Just $ "eq.\\x" <> B.toStrict (TL.encodeUtf8 hexHandleName))
  ]

handleHandler :: TL.Text -> ReaderT Config Handler TL.Text
handleHandler inputHandle = do
  cfg <- ask
  request' <- parseRequest $ serverHost cfg ++ "/ma_tx_out"
  let request = setRequestQueryString (query $ removeDollar inputHandle)
              $ setRequestSecure (https cfg)
              $ request'
      portSetRequest = case serverPort cfg of
        Nothing -> request
        Just p  -> setRequestPort p request
  response <- httpJSON portSetRequest
  case fromJSON $ getResponseBody response :: Result [RawHandleAddr] of
    Error _         -> throwError $ err500 {errBody = "Internal server error"}
    Success []      -> throwError $ err404 {errBody = "Handle not found"}
    Success (x : _) -> pure $ getAddr x

removeDollar :: TL.Text -> TL.Text
removeDollar handleName =
  let tailHandleName = case TL.head handleName of
        '$' -> TL.tail handleName
        _   -> handleName
   in TL.fromStrict $ encodeHex $ B.toStrict $ TL.encodeUtf8 tailHandleName
