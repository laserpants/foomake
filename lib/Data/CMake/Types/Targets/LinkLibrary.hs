{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.LinkLibrary
  ( LinkLibrary(..)
  , linkLibraries
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (isNothing)
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
linkLibraries v = do
  orign <- v .:? "linkLibraries"
  alias <- v .:? "linkLibs"
  guard (isNothing orign || isNothing alias)
  pure (orign <|> alias) .!= []
