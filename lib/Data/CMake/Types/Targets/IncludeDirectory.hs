{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.IncludeDirectory
  ( IncludeDirectory(..)
  , parseIncludeDirectories
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.CMake.Types.Targets.Scope
import Data.CMake.Utils
import Data.Maybe (maybeToList)
import Data.Text

data IncludeDirectory = IncludeDirectory
  { path         :: !Text
  , includeScope :: !Scope
  } deriving (Eq, Show)

instance FromJSON IncludeDirectory where
  parseJSON (String s) = pure (IncludeDirectory s Unspecified)
  parseJSON (Object v) =
    IncludeDirectory <$> v .: "path"
                     <*> pure Unspecified
  parseJSON _ = fail "‘include-directories’ list entries must be strings or objects"

instance HasScope IncludeDirectory where
  setScope scope dir = dir{ includeScope = scope }

parseIncludeDirectories :: Object -> Parser [IncludeDirectory]
parseIncludeDirectories v = join . maybeToList <$> parser where
    parser :: Parser (Maybe [IncludeDirectory])
    parser = fmap fmap fmap ungroup (prop v) <|> prop v
    prop v = parseAlias v "include-directories" "include-dirs"
