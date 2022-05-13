{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GetHandle (getHandle) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Aeson (Value, withArray, withObject, (.:))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString as BL (toStrict)
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text.Lazy as TL (Text, fromStrict, head, tail)
import Data.Text.Lazy.Encoding as TL (encodeUtf8)
import Data.Vector (Vector, fromList, head, null)
import Network.HTTP.Req (Option, Scheme (Http, Https), Url, (=:))
import SendDbReq (sendReq)
import Text.Hex (encodeHex)
import Text.URI

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
    Left errStr -> throwE 404
    Right v -> maybe (throwE 404) (return) $ fromJust $ parseMaybe valParse v

removeDollar :: Text -> Text
removeDollar handleName =
  let tailHandleName = case TL.head handleName of
        '$' -> TL.tail handleName
        _ -> handleName
   in fromStrict $ encodeHex $ BL.toStrict $ TL.encodeUtf8 $ tailHandleName

valParse :: Value -> Parser (Maybe Text)
valParse = withArray "Response" arrayParse

arrayParse :: Vector Value -> Parser (Maybe Text)
arrayParse arr = do
  let vallst = Data.Vector.fromList $ Data.Foldable.toList arr
  case Data.Vector.null $ vallst of
    True -> pure Nothing
    False -> do
      headElem <- withObject "Response[..]" pure $ Data.Vector.head vallst
      return $ parseMaybe (.: "address") $ fromJust $ parseMaybe (.: "tx_out") headElem
