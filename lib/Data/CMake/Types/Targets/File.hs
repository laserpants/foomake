{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.File 
  ( File(..) 
  ) where

import Data.Aeson
import Data.Text

data File = File
  { fileName :: !Text
  } deriving (Eq, Show)

instance FromJSON File where
  parseJSON (String s) = pure (File s)
  parseJSON (Object v) =
    File <$> v .: "name"
  parseJSON _ = fail "‘source-files’ list entries must be strings or objects"
