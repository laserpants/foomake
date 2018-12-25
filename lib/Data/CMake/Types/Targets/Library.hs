{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.Library 
  ( Library(..)
  ) where

import Data.Aeson
import Data.CMake.Types.Targets.File
import Data.CMake.Types.Targets.IncludeDirectory
import Data.CMake.Types.Targets.LinkLibrary
import Data.Text

data LibraryType = Static | Shared | Module
  deriving (Eq, Show)

instance FromJSON LibraryType where
  parseJSON (String "shared") = pure Shared
  parseJSON (String "SHARED") = pure Shared
  parseJSON (String "static") = pure Static
  parseJSON (String "STATIC") = pure Static
  parseJSON (String "module") = pure Module
  parseJSON (String "MODULE") = pure Module
  parseJSON (String _) = fail "unrecognized library type"
  parseJSON _ = fail "‘library.type’ must be a string"

data Library = Library
  { typeof      :: !LibraryType
  , includeDirs :: ![IncludeDirectory]
  , linkLibs    :: ![LinkLibrary]
  , files       :: ![File]
  } deriving (Eq, Show)

instance FromJSON Library where
  parseJSON (Object v) =
    Library <$> v .:? "type" .!= Static
            <*> includeDirectories v
            <*> linkLibraries v
            <*> v .:? "files" .!= []
  parseJSON _ = fail "‘libraries’ list entries must be objects"
