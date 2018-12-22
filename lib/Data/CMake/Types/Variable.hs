{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Variable
  ( Variable(..) 
  ) where

import Data.Aeson
import Data.Text

data Variable = Variable
  { value :: !Text
  } deriving (Eq, Show)

instance FromJSON Variable where
  parseJSON (String str) = pure (Variable str)
  parseJSON (Object v) =
    Variable <$> v .: "value"
  parseJSON _ = fail "variable value must be either a string or an object"
