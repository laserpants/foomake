{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Config where

import Data.Aeson
import Data.CMake.Types.CMake
import Data.CMake.Types.Install
import Data.CMake.Types.Project
import Data.CMake.Types.Targets
import Data.CMake.Types.Variables

data Config = Config
--  { project   :: !(Maybe Project)
--  , cmake     :: !(Maybe CMake)
--  , variables :: !(Maybe Variables)
--  , install   :: !(Maybe Install)
--  , targets   :: !Targets
--  } deriving (Eq, Show)
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) = pure Config
--    Config <$> v .:? "project"
--           <*> v .:? "cmake"
--           <*> v .:? "variables"
--           <*> v .:? "install"
--           <*> v .:? "targets" .!= Targets [] []
  parseJSON _ = fail "configuration must be an object"
