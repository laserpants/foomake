{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.LinkLibrary
  ( LinkLibrary(..)
  , parseLinkLibraries
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.CMake.Utils
import Data.Text

data LinkLibrary = LinkLibrary
  { library :: !Text
  } deriving (Eq, Show)

instance FromJSON LinkLibrary where
  parseJSON (String s) = pure (LinkLibrary s)
  parseJSON (Object v) =
    LinkLibrary <$> v .: "name"
  parseJSON _ = fail "‘linkLibraries’ list entries must be strings or objects"

parseLinkLibraries :: Object -> Parser [LinkLibrary]
parseLinkLibraries v = parseAlias v "link-libraries" "link-libs" .!= []
