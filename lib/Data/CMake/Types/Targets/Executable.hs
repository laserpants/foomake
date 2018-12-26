{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.Executable 
  ( Executable(..)
  ) where

import Data.Aeson
import Data.CMake.Types.Targets.CompileFeature
import Data.CMake.Types.Targets.File
import Data.CMake.Types.Targets.IncludeDirectory
import Data.CMake.Types.Targets.LinkLibrary
import Data.Text

data Executable = Executable
  { includeDirs    :: ![IncludeDirectory]
  , linkLibs       :: ![LinkLibrary]
  , compileFeature :: ![CompileFeature]
  , files          :: ![File]
  } deriving (Eq, Show)

instance FromJSON Executable where
  parseJSON (Object v) =
    Executable <$> includeDirectories v
               <*> linkLibraries v
               <*> compileFeatures v
               <*> v .:? "files" .!= []
  parseJSON _ = fail "‘executables’ list entries must be objects"
