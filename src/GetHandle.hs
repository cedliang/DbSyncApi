{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GetHandle
  ( getHandle,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Data.Vector
import Network.HTTP.Req
import SendDbReq (sendReq)
import Text.Hex (encodeHex)
import Data.Either (fromRight)

optionScheme :: Text -> Option 'Https
optionScheme hexHandleName =
  "select" =: ("id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)" :: Text)
    <> "order" =: ("id.desc" :: Text)
    <> "limit" =: (1 :: Int)
    <> "multi_asset.policy" =: ("eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" :: Text)
    <> "multi_asset.name" =: ("eq.\\x" <> hexHandleName :: Text)

searchTable :: Text
searchTable = "ma_tx_out"

getHandle :: Text -> IO (Either Int Text)
getHandle handleName = do
  let hexHandleName = encodeHex $
        BL.toStrict $
          TL.encodeUtf8 $ case TL.head handleName of
            '$' -> TL.tail handleName
            _ -> handleName

  thandle <- case handleName of
    "" -> return $ Left 400
    _ -> sendReq (optionScheme $ fromStrict hexHandleName) searchTable

  case thandle of
    Left errStr -> return $ Left errStr
    Right v -> return $ fromJust $ parseMaybe valParse $ v
    
  where
    valParse :: Value -> Parser (Either Int Text)
    valParse = (fmap $ flip maybe Right . Left $ 404) . withArray "Response" arrayParse

    arrayParse :: Vector Value -> Parser (Maybe Text)
    arrayParse arr = do
      let vallst = Data.Vector.fromList $ Data.Foldable.toList arr

      case Data.Vector.null $ vallst of
        True -> pure Nothing
        False -> do
          headElem <- withObject "Response[..]" pure $ Data.Vector.head vallst
          return $ parseMaybe (.: "address") $ fromJust $ parseMaybe (.: "tx_out") headElem
