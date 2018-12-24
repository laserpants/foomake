{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.LinkLibrary
  ( LinkLibrary(..)
  , linkLibraries
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.CMake.Utils
import Data.Text

data LinkLibrary = LinkLibrary
  { libName :: !Text
  } deriving (Eq, Show)

instance FromJSON LinkLibrary where
  parseJSON (String s) = pure (LinkLibrary s)
  parseJSON (Object v) =
    LinkLibrary <$> v .: "name"
  parseJSON _ = fail "‘linkLibraries’ list entries must be strings or objects"

linkLibraries :: Object -> Parser [LinkLibrary]
linkLibraries v = parseAlias v "link-libraries" "link-libs" .!= []
