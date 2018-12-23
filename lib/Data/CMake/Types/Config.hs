{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Config 
  ( Config(..) 
  ) where

import Data.Aeson
import Data.CMake.Types.CMake
import Data.CMake.Types.ConfigureFile
import Data.CMake.Types.Install
import Data.CMake.Types.Option
import Data.CMake.Types.Project
import Data.CMake.Types.Targets
import Data.CMake.Types.Variable
import Data.CMake.Utils
import Data.Monoid (mempty)
import Data.Text

data Config = Config
  { project   :: !Project
  , cmake     :: !CMake
  , targets   :: !Targets
  , variables :: ![(Text, Variable)]
  , options   :: ![(Text, Option)]
  , configure :: ![ConfigureFile]
  , install   :: !Install
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) = do
    variables <- v .:? "variables" .!= mempty
    options   <- v .:? "options"   .!= mempty
    Config <$> parseJSON (Object v)
           <*> parseJSON (Object v)
           <*> parseJSON (Object v)
           <*> parseKvPairs variables
           <*> parseKvPairs options
           <*> v .:? "configure" .!= []
           <*> v .:? "install"   .!= Install
  parseJSON Null = parseJSON (Object mempty)
  parseJSON _ = fail "configuration must be an object"
