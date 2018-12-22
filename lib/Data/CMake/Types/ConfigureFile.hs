{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.ConfigureFile
  ( ConfigureFile(..) 
  ) where

import Control.Monad (guard)
import Data.Aeson
import Data.Text

data ConfigureArg = ConfigureArg
  deriving (Eq, Show)

instance FromJSON ConfigureArg where
  parseJSON _ = pure ConfigureArg

data ConfigureFile = ConfigureFile
  { input     :: !Text
  , output    :: !Text
  , arguments :: ![ConfigureArg]
  } deriving (Eq, Show)

instance FromJSON ConfigureFile where
  parseJSON (Object v) = do
    file <- v .:  "file"
    args <- v .:? "arguments" .!= []
    guard (Prelude.length file == 2)
    pure (ConfigureFile (file !! 0) (file !! 1) args)
  parseJSON _ = fail "‘configure’ list entries must be objects"
