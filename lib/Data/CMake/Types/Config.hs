{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Config 
  ( Config(..) 
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.CMake.Types.CMake
import Data.CMake.Types.ConfigureFile
import Data.CMake.Types.Install
import Data.CMake.Types.Option
import Data.CMake.Types.Project
import Data.CMake.Types.Targets
import Data.CMake.Types.Variable
import Data.HashMap.Strict (HashMap)
import Data.Monoid (mempty)
import Data.Text

import qualified Data.HashMap.Strict as HashMap

data Config = Config
  { project   :: !Project
  , cmake     :: !CMake
  , targets   :: !Targets
  , variables :: ![(Text, Variable)]
  , options   :: ![(Text, Option)]
  , install   :: !Install
  , configure :: ![ConfigureFile]
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
           <*> v .:? "install"   .!= Install
           <*> v .:? "configure" .!= []
  parseJSON Null = parseJSON (Object mempty)
  parseJSON _ = fail "configuration must be an object"

parseKvPairs :: FromJSON a => HashMap Text Value -> Parser [(Text, a)]
parseKvPairs v = sequence (ppair <$> HashMap.toList v) where
    ppair (key, val) = do
      details <- parseJSON val
      pure (key, details)
