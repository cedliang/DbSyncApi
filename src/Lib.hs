{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( mainScotty,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Text.Lazy
import Data.Text.Lazy.Encoding as TL.Encoding
import FizzBuzz (fizzBuzz)
import Network.HTTP.Req
import Network.HTTP.Types (mkStatus, status400)
import Network.HTTP.Types.Method
import Network.Wai.Middleware.Cors
import Text.Read (readMaybe)
import UnliftIO.Exception
import Web.Scotty

mainScotty :: IO ()
mainScotty = scotty 3000 $ do
  middleware simpleCors

  addroute Network.HTTP.Types.Method.GET "/helloworld" $ do
    html "Hello, world!"

  addroute Network.HTTP.Types.Method.GET "/printargs/:arg1/:arg2" $ do
    arg1 <- param "arg1"
    arg2 <- param "arg2"
    html $ arg1 <> fromStrict " " <> arg2

  addroute Network.HTTP.Types.Method.GET "/tx/:txhash" $ do
    inputHash <- param "txhash"
    v <- liftAndCatchIO $ getTx inputHash
    case v of
      ValidHash val -> Web.Scotty.json val
      InvalidHash errStr -> do
        status $ mkStatus 404 $ B.pack "Tx not found."
        text errStr

  addroute Network.HTTP.Types.Method.GET "/adahandle/:handle" $ do
    inputHandle <- param "handle"
    thandle <- liftAndCatchIO $ getHandle inputHandle
    case thandle of
      ValidHandle handle -> Web.Scotty.text handle
      InvalidHandle errStr -> do
        status $ mkStatus 404 $ B.pack "Handle not found."
        text errStr

  addroute Network.HTTP.Types.Method.GET "/fizzbuzz/:arg" $ do
    arg <- param "arg"
    let n = readMaybe $ T.unpack arg :: Maybe Int
    case n of
      Just val -> Web.Scotty.text $ fromStrict $ T.unlines $ fizzBuzz val
      _ -> do
        status status400
        text "Not a valid n"

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
    Right bs -> return $ ValidHandle $ TL.Encoding.decodeUtf8 $ responseBody bs

data AddrInfo
  = ValidHash Value
  | InvalidHash Text
  deriving (Eq, Show)

getTx :: Text -> IO AddrInfo
getTx addr = do
  result <-
    UnliftIO.Exception.try
      ( runReq defaultHttpConfig $ do
          req
            Network.HTTP.Req.GET
            (http "127.0.0.1" /: "api" /: "v1" /: "tx" /~ addr)
            NoReqBody
            jsonResponse
            (port 5000)
      ) ::
      (FromJSON j) => IO (Either HttpException (JsonResponse j))

  case result of
    Left (VanillaHttpException _) -> return $ InvalidHash "\tNot a valid txhash."
    Right v -> return $ ValidHash (responseBody v :: Value)
