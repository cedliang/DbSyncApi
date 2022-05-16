{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GetHandle (getHandle) where

import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson
import Data.ByteString as BL (toStrict)
import Data.Text.Lazy as TL (Text, fromStrict, head, tail)
import Data.Text.Lazy.Encoding as TL (encodeUtf8)
import Network.HTTP.Req (Option, Scheme (Http, Https), Url, (=:))
import SendDbReq (sendReq)
import Text.Hex (encodeHex)

newtype RawHandleAddr = Address TL.Text
  deriving (Show)

instance FromJSON RawHandleAddr where
  parseJSON = withObject "RawhandleAddr" $ \v ->
    Address
      <$> ((.: "tx_out") v >>= (.: "address"))

getAddr :: RawHandleAddr -> TL.Text
getAddr (Address a) = a

optionSchemeHttps :: Text -> Option 'Https
optionSchemeHttps hexHandleName =
  "select" =: ("id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)" :: Text)
    <> "order" =: ("id.desc" :: Text)
    <> "limit" =: (1 :: Int)
    <> "multi_asset.policy" =: ("eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" :: Text)
    <> "multi_asset.name" =: ("eq.\\x" <> hexHandleName :: Text)

optionSchemeHttp :: Text -> Option 'Http
optionSchemeHttp hexHandleName =
  "select" =: ("id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)" :: Text)
    <> "order" =: ("id.desc" :: Text)
    <> "limit" =: (1 :: Int)
    <> "multi_asset.policy" =: ("eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" :: Text)
    <> "multi_asset.name" =: ("eq.\\x" <> hexHandleName :: Text)

searchTable :: Text
searchTable = "ma_tx_out"

getHandle :: Text -> ExceptT Int (ReaderT (Url 'Https) IO) Text
getHandle handleName = do
  when (handleName == "") $ throwE 400

  thandle <- lift $ runExceptT $ sendReq (optionSchemeHttps $ removeDollar handleName) searchTable
  case thandle of
    Left e -> throwE e
    Right v -> do
      let r = fromJSON v :: Result [RawHandleAddr]

      case r of
        Error errStr -> throwE 500
        Success [] -> throwE 404
        Success (x : xs) -> return $ getAddr x

removeDollar :: Text -> Text
removeDollar handleName =
  let tailHandleName = case TL.head handleName of
        '$' -> TL.tail handleName
        _ -> handleName
   in fromStrict $ encodeHex $ BL.toStrict $ TL.encodeUtf8 $ tailHandleName
