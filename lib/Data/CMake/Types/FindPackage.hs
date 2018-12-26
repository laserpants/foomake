{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.FindPackage
  ( FindPackage(..) 
  ) where

import Data.Aeson
import Data.Text

data FindPackage = FindPackage
  { package :: !Text
  } deriving (Eq, Show)

instance FromJSON FindPackage where
  parseJSON (String s) = pure (FindPackage s)
  parseJSON (Object v) =
    FindPackage <$> v .: "package"
  parseJSON _ = fail "‘dependencies’ entries must be objects or strings"
