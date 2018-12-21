{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.IncludeDirectory where

import Data.Aeson
import Data.Text

data IncludeDirectory = IncludeDirectory
  { path  :: !Text
  , scope :: !Text
  } deriving (Eq, Show)

instance FromJSON IncludeDirectory where
  parseJSON (String s) = pure (IncludeDirectory s "public")
  parseJSON (Object v) =
    IncludeDirectory <$> v .:  "path"
                     <*> v .:? "scope" .!= "public"
  parseJSON _ = fail "'includeDirectories' list elements must be strings or objects"
