{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Variables where

import Data.Aeson
import Data.Aeson.Types
import Data.Text

import qualified Data.HashMap.Strict as HashMap

data Var = Var
  { value :: !Text
  } deriving (Eq, Show)

instance FromJSON Var where
  parseJSON (String str) = pure (Var str)
  parseJSON (Object v) =
    Var <$> v .: "value"
  parseJSON _ = fail "variable value must be either a string or an object"

newtype Variables = Variables [(Text, Var)]
  deriving (Eq, Show)

parseVariables :: [(Text, Value)] -> Parser Variables
parseVariables pairs = fmap Variables (sequence (fmap ppair pairs))
  where
    ppair (key, val) = do
      details <- parseJSON val
      pure (key, details)

instance FromJSON Variables where
  parseJSON (Object v) = parseVariables (HashMap.toList v)
  parseJSON _ = fail "‘variables’ must be an object"
