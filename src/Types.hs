module Types where

data Config = Config
  { hostPort   :: Int,
    https      :: Bool,
    serverHost :: String,
    serverPort :: Maybe Int
  }
  deriving (Show, Eq)
