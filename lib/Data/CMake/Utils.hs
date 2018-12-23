{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Utils 
  ( parseAlias
  , parseKvPairs
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isNothing)
import Data.Text

import qualified Data.HashMap.Strict as HashMap

parseAlias :: FromJSON a => Object -> Text -> Text -> Parser (Maybe a)
parseAlias v orign alias = do
  fst <- v .:? orign
  snd <- v .:? alias
  guard (isNothing fst || isNothing snd)
  pure (fst <|> snd)

parseKvPairs :: FromJSON a => HashMap Text Value -> Parser [(Text, a)]
parseKvPairs v = sequence (ppair <$> HashMap.toList v) where
    ppair (key, val) = do
      details <- parseJSON val
      pure (key, details)
