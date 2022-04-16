{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GetHandle
  ( getHandle,
    HandleInfo (ValidHandle, InvalidHandle),
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Text.Lazy
import Data.Text.Lazy.Encoding as TL.Encoding
import Data.Vector
import Network.HTTP.Req
import Network.HTTP.Types (mkStatus, status400)
import SendDbReq (ReqResponse (InvalidResponse, ValidResponse), sendReq)
import Text.Hex (encodeHex)
import Text.Read (readMaybe)
import UnliftIO.Exception
import Web.Scotty (liftAndCatchIO)

data HandleInfo
  = ValidHandle Text
  | InvalidHandle Text
  deriving (Eq, Show)

optionScheme :: Text -> Option 'Https
optionScheme hexHandleName =
  "select" =: ("id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)" :: Text)
    <> "order" =: ("id.desc" :: Text)
    <> "limit" =: (1 :: Int)
    <> "multi_asset.policy" =: ("eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" :: Text)
    <> "multi_asset.name" =: ("eq.\\x" <> hexHandleName :: Text)

getHandle :: Text -> IO HandleInfo
getHandle handleName = do
  let hexHandleName = case Data.Text.Lazy.head handleName of
        '$' -> encodeHex $ BL.toStrict $ TL.Encoding.encodeUtf8 $ Data.Text.Lazy.tail handleName
        _ -> encodeHex $ BL.toStrict $ TL.Encoding.encodeUtf8 handleName
  let reqString = hexHandleName

  thandle <- liftIO $ sendReq $ optionScheme $ fromStrict reqString

  case thandle of
    ValidResponse val ->
      let parseOutcome = fromJust $ parseMaybe valParse val
       in case parseOutcome of
            Nothing -> return $ InvalidHandle "\tNot a valid handle."
            Just hdl -> return $ ValidHandle hdl
    InvalidResponse errorMessage -> return $ InvalidHandle "\tNot a valid handle."
  where
    valParse :: Value -> Parser (Maybe Text)
    valParse = withArray "Response" arrayParse

    arrayParse :: Vector Value -> Parser (Maybe Text)
    arrayParse arr = do
      let vallst = Data.Vector.fromList $ Data.Foldable.toList arr

      if Data.Vector.null vallst
        then return Nothing
        else do
          headElem <- withObject "Response[..]" pure $ Data.Vector.head vallst
          let firstLayer = fromJust $ parseMaybe (.: "tx_out") headElem :: Object
          let returnText = fromJust $ parseMaybe (.: "address") firstLayer :: Text
          return $ Just returnText
