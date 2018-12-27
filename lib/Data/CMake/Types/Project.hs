{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Project
  ( Project(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)

import qualified Data.HashMap.Strict as HashMap

data Project = Project
  { name        :: !(Maybe Text)
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
  parseJSON (Object v) = do
    Languages languages <- v .:? "languages" .!= Languages []
    name                <- v .:? "name"
    version             <- v .:? "version"
    description         <- v .:? "description"
    homepage            <- v .:? "homepage"
    let allNothing = null languages && isNothing version && isNothing description && isNothing homepage
    -- name must be set if any other project property is given
    guard (allNothing || isJust name)
    pure (Project name version description homepage languages)
  parseJSON _ = fail "application error"
