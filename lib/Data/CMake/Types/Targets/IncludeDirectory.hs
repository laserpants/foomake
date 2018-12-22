{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.IncludeDirectory where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (isNothing)
import Data.Text

data Scope = Public | Private | Interface
  deriving (Eq, Show)

instance FromJSON Scope where
  parseJSON (String "public") = pure Public
  parseJSON (String "PUBLIC") = pure Public
  parseJSON (String "private") = pure Private
  parseJSON (String "PRIVATE") = pure Private
  parseJSON (String "interface") = pure Interface
  parseJSON (String "INTERFACE") = pure Interface
  parseJSON (String _) = fail "unrecognized include directory scope"
  parseJSON _ = fail "scope must be a string"

data IncludeDirectory = IncludeDirectory
  { path  :: !Text
  , scope :: !Scope
  } deriving (Eq, Show)

instance FromJSON IncludeDirectory where
  parseJSON (String s) = pure (IncludeDirectory s Public)
  parseJSON (Object v) =
    IncludeDirectory <$> v .:  "path"
                     <*> v .:? "scope" .!= Public
  parseJSON _ = fail "‘includeDirectories’ list entries must be strings or objects"

includeDirectories :: Object -> Parser [IncludeDirectory]
includeDirectories v = do
  orign <- v .:? "includeDirectories"
  alias <- v .:? "includeDirs"
  guard (isNothing orign || isNothing alias)
  pure (orign <|> alias) .!= []
