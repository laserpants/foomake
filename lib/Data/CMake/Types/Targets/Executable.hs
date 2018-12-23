{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.Executable 
  ( Executable(..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.CMake.Types.Targets.IncludeDirectory
import Data.CMake.Types.Targets.LinkLibrary
import Data.Text

import Data.Aeson.Types

data Executable = Executable
  { includeDirs :: ![IncludeDirectory]
  , linkLibs    :: ![LinkLibrary]
  } deriving (Eq, Show)

instance FromJSON Executable where
  parseJSON (Object v) =
    Executable <$> includeDirectories v
               <*> linkLibraries v
  parseJSON _ = fail "‘executables’ list entries must be objects"
