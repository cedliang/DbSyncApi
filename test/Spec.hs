{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text.Lazy as TL
import GetHandle (getHandle)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = do
  let hs = [("ced" :: TL.Text), "44", "1", ""]
  qs <- mapM (runExceptT . getHandle) hs

  let as =
        [ (Right ("addr1q92pux42qwvp5rygjngp6xt9rsx5vcv2s49ukxnce63j99grd5rswpndufeuk8zrf39mawlp2c0dqemstgqsa2te8sksx5k49q" :: TL.Text)),
          Right "addr1q888888dp8rldgev66hkqe372km58j5msypgp0pwzay0377zmrdnn9s9y3zqmwf9f90rgqafjq48ulwqmmq3yphu2g8q4092ar",
          Left 404,
          Left 400
        ]

  defaultMain (testGroup "Lib Tests" [handleTests qs as])

handleTests :: (Eq a, Show a) => [a] -> [a] -> TestTree
handleTests qs as =
  testGroup "Handle tests" $ map (testCase "correct handles") $ zipWith (assertEqual "should equate") qs as
