{-# LANGUAGE OverloadedStrings #-}

module GetHandle
  ( getHandle,
    getHandleNew,
    HandleInfo (ValidHandle, InvalidHandle),
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Text.Lazy
import Data.Text.Lazy.Encoding as TL.Encoding
import Network.HTTP.Req
import Network.HTTP.Types (mkStatus, status400)
import Text.Hex (encodeHex)
import Text.Read (readMaybe)
import UnliftIO.Exception
import Web.Scotty (liftAndCatchIO)
import SendDbReq ( sendReq, ReqResponse (ValidResponse, InvalidResponse) )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Aeson.Types
import Data.Foldable (toList)
import Data.Vector


data HandleInfo
  = ValidHandle Text
  | InvalidHandle Text
  deriving (Eq, Show)

getHandle :: Text -> IO HandleInfo
getHandle handleName = do
  result <-
    UnliftIO.Exception.try
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (http "127.0.0.1" /: "api" /: "v1" /: "adahandle" /~ handleName)
            NoReqBody
            lbsResponse
            (port 5000)
      )

  case result of
    Left (VanillaHttpException _) -> return $ InvalidHandle "\tNot a valid handle."
    Left _ -> return $ InvalidHandle "\tNot a valid handle."
    Right bs -> return $ ValidHandle $ TL.Encoding.decodeUtf8 $ responseBody bs


getHandleNew :: Text -> IO HandleInfo
getHandleNew handleName = do
  let hexHandleName = case Data.Text.Lazy.head handleName of
        '$' -> encodeHex $ BL.toStrict $ TL.Encoding.encodeUtf8 $ Data.Text.Lazy.tail handleName
        _ -> encodeHex $ BL.toStrict $ TL.Encoding.encodeUtf8 handleName
  let reqString = "ma_tx_out?select=id,tx_out!inner(id,address),multi_asset!inner(id,policy,name)&order=id.desc&limit=1&multi_asset.policy=eq.\\xf0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a&multi_asset.name=eq.\\x" <> hexHandleName

  thandle <- liftIO $ sendReq $ fromStrict reqString

  case thandle of 
    ValidResponse val -> return $ ValidHandle $ fromJust $ parseMaybe f val
    InvalidResponse errorMessage -> return $ InvalidHandle "\tNot a valid handle."

  where
    f = withArray "Response" arrayParse

    arrayParse :: Vector Value -> Parser Text
    arrayParse arr = do
        let vallst = Data.Vector.fromList $ Data.Foldable.toList arr
        headElem <- withObject "Response[..]" pure $ Data.Vector.head vallst
        let firstLayer = fromJust $ parseMaybe (.: "tx_out") headElem :: Object
        let returnText = fromJust $ parseMaybe (.: "address") firstLayer :: Text
        return returnText
