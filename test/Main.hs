{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.CMake.Types
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Text
import Test.Hspec

import qualified Data.ByteString as ByteString
import qualified Data.Yaml as Yaml

shouldBeLeft :: Either a b -> Expectation
shouldBeLeft = shouldBe True . isLeft

withRightExpect :: Either Yaml.ParseException a -> (a -> Expectation) -> Expectation
withRightExpect either expect = 
  case either of
    Left  e -> expectationFailure (show e)
    Right r -> expect r

main :: IO ()
main = do

  hspec $ do

    -- name: test
    -- version: 0.1.3

    describe "name: test" $ do

      it "should set name to Just \"test\"" $ 
        let conf = "name: test\nversion: 0.1.3"
            result = Yaml.decodeEither' conf :: Either Yaml.ParseException Config
         in withRightExpect result (\config -> name (project config) `shouldBe` Just "test")

    -- name: [1]
    -- version: 0.1.3

    describe "name: [1]" $ do

      it "should fail to parse" $ 
        let conf = "name: [1]\nversion: 0.1.3"
            result = Yaml.decodeEither' conf :: Either Yaml.ParseException Config
         in shouldBeLeft result

    -- version: 0.1.3

    describe "name: -" $ do

      it "should set name to Nothing" $ 
        let conf = "version: 0.1.3"
            result = Yaml.decodeEither' conf :: Either Yaml.ParseException Config
         in withRightExpect result (\config -> name (project config) `shouldBe` Nothing)

