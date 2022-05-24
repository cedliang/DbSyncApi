{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server (servantIO) where

import qualified Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Maybe
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Data.Time.Calendar
import           GetHandle
import           GHC.Generics
import           Network.HTTP.Req
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           System.Exit
import           Text.URI                             as TU

data Config = Config
  { hostPort  :: Int,
    serverUrl :: T.Text
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config {hostPort = 4000, serverUrl = "https://cedric.app/api/dbsync/postgrest"}

getUrlFromConfig :: IO (Url 'Https)
getUrlFromConfig = do
  serverUri <- Control.Exception.catch (mkURI $ serverUrl defaultConfig) (const $ die "Invalid URL cannot be parsed" :: ParseException -> IO TU.URI)
  let rawServerUrl = useHttpsURI serverUri
  when (isNothing rawServerUrl) (die "Invalid URL cannot be parsed")
  pure $ fst $ fromJust rawServerUrl

servantIO :: IO ()
servantIO = do
  queryUrl <- getUrlFromConfig
  run (hostPort defaultConfig) $ (simpleCors . logStdoutDev) $ servApp queryUrl

servApp :: Url 'Https -> Application
servApp = serve (Proxy :: Proxy CardanoAPI) . server1

type CardanoAPI = "adahandle" :> Capture "inputHandle" TL.Text :> Get '[PlainText] TL.Text

server1 :: Url 'Https -> ServerT CardanoAPI Handler
server1 queryUrl = flip runReaderT queryUrl . handleHandler
