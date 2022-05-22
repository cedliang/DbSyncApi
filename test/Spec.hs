{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Text.Lazy qualified as TL
import GetHandle
import Server
import Test.Tasty
import Test.Tasty.HUnit
import Text.URI

main :: IO ()
main = do
  queryUrl <- getUrlFromConfig

  let hs =
        [ "ced" :: TL.Text,
          "44",
          "1",
          ""
        ]

      as =
        [ Right ("addr1q92pux42qwvp5rygjngp6xt9rsx5vcv2s49ukxnce63j99grd5rswpndufeuk8zrf39mawlp2c0dqemstgqsa2te8sksx5k49q" :: TL.Text),
          Right "addr1q888888dp8rldgev66hkqe372km58j5msypgp0pwzay0377zmrdnn9s9y3zqmwf9f90rgqafjq48ulwqmmq3yphu2g8q4092ar",
          Left 404,
          Left 400
        ]

      names =
        [ "correct handle fetch 1: ced",
          "correct handle fetch 2: 44",
          "non-existent handle check 1",
          "empty query check 1"
        ]

  qs <- traverse (flip runReaderT queryUrl . runExceptT . getHandle) hs
  defaultMain (handleTests qs as names)

handleTests :: (Eq a, Show a) => [a] -> [a] -> [String] -> TestTree
handleTests qs as names =
  testGroup "Handle tests" $ zipWith (curry nameAndTestCase) names (zipWith (assertEqual "should equate") qs as)

nameAndTestCase :: (String, Assertion) -> TestTree
nameAndTestCase tup = testCase (show $ fst tup) $ snd tup