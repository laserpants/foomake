{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.Executable where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.CMake.Types.Targets.IncludeDirectory
import Data.Text

data Executable = Executable
  { includeDirs :: ![IncludeDirectory]
  } deriving (Eq, Show)

instance FromJSON Executable where
  parseJSON (Object v) =
    Executable <$> (v .: "includeDirectories" <|> v .: "includeDirs" <|> pure [])
  parseJSON _ = fail "'executables' list elements must be objects"
