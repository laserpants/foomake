{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.CMake where

import Data.Aeson
import Data.Text

data MinimumRequired = MinimumRequired
  { cmakeVersion :: !Text
  } deriving (Eq, Show)

instance FromJSON MinimumRequired where
  parseJSON (Object v) = MinimumRequired <$> v .: "version"
  parseJSON (String s) = MinimumRequired <$> pure s
  parseJSON _ = fail "'cmakeMinimumRequired' must be an object or a string"

data CMake = CMake
  { minimumRequired :: !(Maybe MinimumRequired)
  } deriving (Eq, Show)

instance FromJSON CMake where
  parseJSON (Object v) =
    CMake <$> v .:? "cmakeMinimumRequired"
  parseJSON _ = fail "application error"
