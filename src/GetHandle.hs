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

<<<<<<< HEAD
newtype RawHandleAddr = Address { getAddr :: TL.Text }
=======
newtype RawHandleAddr = Address {getAddr :: TL.Text}
>>>>>>> servant
  deriving (Show)

instance FromJSON RawHandleAddr where
  parseJSON = withObject "RawhandleAddr" $ fmap Address . (.: "address") <=< (.: "tx_out")

<<<<<<< HEAD
optionSchemeHttps :: TL.Text -> Option 'Https
optionSchemeHttps hexHandleName =
  "select" =: ("id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)" :: TL.Text)
    <> "order" =: ("id.desc" :: TL.Text)
    <> "limit" =: (1 :: Int)
    <> "multi_asset.policy" =: ("eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" :: TL.Text)
    <> "multi_asset.name" =: ("eq.\\x" <> hexHandleName :: TL.Text)

optionSchemeHttp :: TL.Text -> Option 'Http
optionSchemeHttp hexHandleName =
  "select" =: ("id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)" :: TL.Text)
    <> "order" =: ("id.desc" :: TL.Text)
    <> "limit" =: (1 :: Int)
    <> "multi_asset.policy" =: ("eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" :: TL.Text)
    <> "multi_asset.name" =: ("eq.\\x" <> hexHandleName :: TL.Text)

searchTable :: TL.Text
searchTable = "ma_tx_out"

getHandle :: TL.Text -> ExceptT Int (ReaderT (Url 'Https) IO) TL.Text
getHandle handleName = do
  when (handleName == "") $ throwE 400
  thandle <- lift $ runExceptT $ sendReq (optionSchemeHttps $ removeDollar handleName) searchTable
  case thandle of
    Left e -> throwE e
    Right v -> case fromJSON v :: Result [RawHandleAddr] of
      Error errStr     -> throwE 500
      Success []       -> throwE 404
      Success (x : xs) -> pure $ getAddr x
=======
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
>>>>>>> servant

removeDollar :: TL.Text -> TL.Text
removeDollar handleName =
  let tailHandleName = case TL.head handleName of
        '$' -> TL.tail handleName
        _   -> handleName
   in TL.fromStrict $ encodeHex $ B.toStrict $ TL.encodeUtf8 tailHandleName
