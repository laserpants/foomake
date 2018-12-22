{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Option
  ( Option(..)
  ) where

import Data.Aeson
import Data.Text

data Option = Option
  { optDescription :: !(Maybe Text)
  , initialValue   :: !(Maybe Text)
  } deriving (Eq, Show)

instance FromJSON Option where
  parseJSON (Object v) =
    Option <$> v .:? "description"
           <*> v .:? "initialValue"
  parseJSON Null = parseJSON (Object mempty)
  parseJSON _ = fail "‘options’ entries must be objects"
