{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Project
  ( Project(..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Text

import qualified Data.HashMap.Strict as HashMap

data Project = Project
  { name        :: !Text
  , version     :: !(Maybe Text)
  , description :: !(Maybe Text)
  , homepage    :: !(Maybe Text)
  , languages   :: ![Text]
  } deriving (Eq, Show)

newtype Languages = Languages { unLanguages :: [Text] }
  deriving (Eq, Show)

instance FromJSON Languages where
  parseJSON (Array v)  = Languages <$> parseJSON (Array v)
  parseJSON (String s) = pure (Languages [s])
  parseJSON _ = fail "‘languages’ must be an array or a string"

instance FromJSON Project where
  parseJSON (Object v) =
    let languages = v .:? "languages" .!= Languages []
     in Project <$> v .:  "name"
                <*> v .:? "version"
                <*> v .:? "description"
                <*> v .:? "homepage"
                <*> fmap unLanguages languages
  parseJSON _ = fail "application error"
