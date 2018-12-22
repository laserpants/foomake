{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Configure
  ( Configure(..) 
  ) where

import Data.Aeson
import Data.Text

data ConfigureArg = ConfigureArg
  deriving (Eq, Show)

instance FromJSON ConfigureArg where
  parseJSON _ = pure ConfigureArg

data Configure = Configure
  { input     :: !Text
  , output    :: !Text
  , arguments :: ![ConfigureArg]
  } deriving (Eq, Show)

instance FromJSON Configure where
  parseJSON (Object v) =
    Configure <$> v .: "input"
              <*> v .: "output"
              <*> v .: "arguments"
  parseJSON _ = fail "‘configure’ must be an object"
