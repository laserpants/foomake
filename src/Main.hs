module Main where

import Data.CMake.Types

import qualified Data.ByteString as ByteString
import qualified Data.Yaml as Yaml

main :: IO ()
main = do
  file <- ByteString.readFile "tmp.yaml"
  config <- Yaml.decodeThrow file
  print (config :: Config)
