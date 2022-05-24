{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module GetHandle (handleHandler) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Network.HTTP.Req
import           SendDbReq
import           Servant
import           Text.Hex

newtype RawHandleAddr = Address {getAddr :: TL.Text}
  deriving (Show)

instance FromJSON RawHandleAddr where
  parseJSON = withObject "RawhandleAddr" $ fmap Address . (.: "address") <=< (.: "tx_out")

optionScheme :: TL.Text -> (Option 'Http, Option 'Https)
optionScheme hexHandleName =
  ( "select" =: ("id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)" :: TL.Text)
      <> "order" =: ("id.desc" :: TL.Text)
      <> "limit" =: (1 :: Int)
      <> "multi_asset.policy" =: ("eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" :: TL.Text)
      <> "multi_asset.name" =: ("eq.\\x" <> hexHandleName :: TL.Text),
    "select" =: ("id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)" :: TL.Text)
      <> "order" =: ("id.desc" :: TL.Text)
      <> "limit" =: (1 :: Int)
      <> "multi_asset.policy" =: ("eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" :: TL.Text)
      <> "multi_asset.name" =: ("eq.\\x" <> hexHandleName :: TL.Text)
  )

searchTable :: TL.Text
searchTable = "ma_tx_out"

handleHandler :: TL.Text -> ReaderT QueryServerConfig Handler TL.Text
handleHandler inputHandle = do
  thandle <- sendReq (optionScheme $ removeDollar inputHandle) searchTable
  case fromJSON thandle :: Result [RawHandleAddr] of
    Error _         -> throwError $ err500 {errBody = "Internal server error"}
    Success []      -> throwError $ err404 {errBody = "Handle not found"}
    Success (x : _) -> pure $ getAddr x

removeDollar :: TL.Text -> TL.Text
removeDollar handleName =
  let tailHandleName = case TL.head handleName of
        '$' -> TL.tail handleName
        _   -> handleName
   in TL.fromStrict $ encodeHex $ B.toStrict $ TL.encodeUtf8 tailHandleName
