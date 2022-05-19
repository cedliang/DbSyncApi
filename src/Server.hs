{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( mainScotty,
    getUrlFromConfig,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.ByteString.Char8 as B
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GetHandle
import GetTx
import Network.HTTP.Req
import Network.HTTP.Types
import Network.HTTP.Types.Method
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import System.Exit
import Text.URI
import Web.Scotty

data Config = Config
  { hostPort :: Int,
    serverUrl :: T.Text
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config {hostPort = 3000, serverUrl = "https://cedric.app/api/dbsync/postgrest"}

getUrlFromConfig :: IO (Url 'Https)
getUrlFromConfig = do
  serverUri <- Control.Exception.catch (mkURI $ serverUrl defaultConfig) (const $ die "Invalid URL cannot be parsed" :: ParseException -> IO URI)
  let rawServerUrl = useHttpsURI serverUri
  when (isNothing rawServerUrl) (die "Invalid URL cannot be parsed")
  return $ fst $ fromJust rawServerUrl

mainScotty :: IO ()
mainScotty = do
  queryUrl <- getUrlFromConfig

  scotty (hostPort defaultConfig) $ do
    middleware simpleCors
    middleware logStdout

    addroute Network.HTTP.Types.Method.GET "/tx/:txhash" $ do
      inputHash <- param "txhash"
      v <- liftAndCatchIO $ runExceptT $ getTx inputHash
      case v of
        Right val -> Web.Scotty.json val
        Left errStr -> do
          status $ mkStatus 404 $ B.pack "Tx not found."
          text errStr

    addroute Network.HTTP.Types.Method.GET "/adahandle/:handle" $ do
      inputHandle <- param "handle"
      thandle <- liftAndCatchIO $ runReaderT (runExceptT $ getHandle inputHandle) queryUrl
      case thandle of
        Right handle -> Web.Scotty.text handle
        Left sCode -> do
          let errStr = case sCode of
                400 -> "Bad request"
                404 -> "Handle not found"
                500 -> "Internal server error"
          status $ mkStatus sCode $ B.pack errStr
          text $ TL.pack $ show sCode ++ ": " ++ errStr
