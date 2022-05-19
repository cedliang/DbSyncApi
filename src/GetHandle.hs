{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module GetHandle (getHandle) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.ByteString qualified as B
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Network.HTTP.Req
import SendDbReq
import Text.Hex

newtype RawHandleAddr = Address TL.Text
  deriving (Show)

instance FromJSON RawHandleAddr where
  parseJSON = withObject "RawhandleAddr" $ \v ->
    Address
      <$> ((.: "tx_out") v >>= (.: "address"))

getAddr :: RawHandleAddr -> TL.Text
getAddr (Address a) = a

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
      Error errStr -> throwE 500
      Success [] -> throwE 404
      Success (x : xs) -> pure $ getAddr x

removeDollar :: TL.Text -> TL.Text
removeDollar handleName =
  let tailHandleName = case TL.head handleName of
        '$' -> TL.tail handleName
        _ -> handleName
   in TL.fromStrict $ encodeHex $ B.toStrict $ TL.encodeUtf8 tailHandleName
