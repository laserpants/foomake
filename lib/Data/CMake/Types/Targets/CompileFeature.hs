{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.CompileFeature 
  ( CompileFeature(..)
  , parseCompileFeatures
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.CMake.Types.Targets.Scope
import Data.Maybe (maybeToList)
import Data.Text

data CompileFeature = CompileFeature
  { feature      :: !Text
  , featureScope :: !Scope
  } deriving (Eq, Show)

instance FromJSON CompileFeature where
  parseJSON (String s) = pure (CompileFeature s Unspecified)
  parseJSON (Object v) =
    CompileFeature <$> v .: "feature"
                   <*> pure Unspecified
  parseJSON _ = fail "‘compile-features’ list entries must be strings or objects"

instance HasScope CompileFeature where
  setScope scope feature = feature{ featureScope = scope }

parseCompileFeatures :: Object -> Parser [CompileFeature]
parseCompileFeatures v = join . maybeToList <$> parser where
    parser :: Parser (Maybe [CompileFeature])
    parser = fmap fmap fmap ungroup (prop v) <|> prop v
    prop v = v .:? "compile-features"
