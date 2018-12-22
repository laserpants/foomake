{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Install 
  ( Install(..)
  ) where

import Data.Aeson
import Data.Text

data Install = Install
  deriving (Eq, Show)

instance FromJSON Install where
  parseJSON _ = pure Install
