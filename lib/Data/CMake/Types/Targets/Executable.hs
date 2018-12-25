{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.Executable 
  ( Executable(..)
  ) where

import Data.Aeson
import Data.CMake.Types.Targets.File
import Data.CMake.Types.Targets.IncludeDirectory
import Data.CMake.Types.Targets.LinkLibrary
import Data.Text

data Executable = Executable
  { includeDirs :: ![IncludeDirectory]
  , linkLibs    :: ![LinkLibrary]
  , files       :: ![File]
  } deriving (Eq, Show)

instance FromJSON Executable where
  parseJSON (Object v) =
    Executable <$> includeDirectories v
               <*> linkLibraries v
               <*> v .:? "files" .!= []
  parseJSON _ = fail "‘executables’ list entries must be objects"
