{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.ConfigureFile
  ( ConfigureFile(..) 
  ) where

import Data.Aeson
import Data.Text

data ConfigureArg = ConfigureArg
  deriving (Eq, Show)

instance FromJSON ConfigureArg where
  parseJSON _ = pure ConfigureArg

data ConfigureFile = ConfigureFile
  { input     :: !Text
  , output    :: !Text
  , arguments :: ![ConfigureArg]
  } deriving (Eq, Show)

instance FromJSON ConfigureFile where
  parseJSON (Object v) =
    ConfigureFile <$> v .: "input"
                  <*> v .: "output"
                  <*> v .: "arguments"
  parseJSON _ = fail "‘configure’ list entries must be objects"
