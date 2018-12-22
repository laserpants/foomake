{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Config where

import Data.Aeson
import Data.Aeson.Types
import Data.CMake.Types.CMake
import Data.CMake.Types.Install
import Data.CMake.Types.Project
import Data.CMake.Types.Targets
import Data.CMake.Types.Variables
import Data.Monoid (mempty)
import Data.Text

data Config = Config
  { project   :: !Project
  , cmake     :: !CMake
  , targets   :: !Targets
  , variables :: !Variables
  , install   :: !Install
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> parseJSON (Object v)
           <*> parseJSON (Object v)
           <*> parseJSON (Object v)
           <*> v .:? "variables" .!= Variables []
           <*> v .:? "install"   .!= Install
  parseJSON Null = parseJSON (Object mempty)
  parseJSON _ = fail "configuration must be an object"
