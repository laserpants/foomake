{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets
  ( Targets(..)
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.CMake.Types.Targets.Executable
import Data.CMake.Types.Targets.Library
import Data.Text

import qualified Data.HashMap.Strict as HashMap

data Targets = Targets
  { executables :: ![(Text, Executable)]
  , libraries   :: ![(Text, Library)]
  } deriving (Eq, Show)

instance FromJSON Targets where
  parseJSON (Object v) =
    let toList' = fmap HashMap.toList
     in Targets <$> toList' (v .:? "executables" .!= HashMap.empty)
                <*> toList' (v .:? "libraries"   .!= HashMap.empty)
  parseJSON _ = fail "application error"
