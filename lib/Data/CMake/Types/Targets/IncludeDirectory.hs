{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.IncludeDirectory where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Text

--data Scope = Public | Private | ?

data IncludeDirectory = IncludeDirectory
  { path  :: !Text
  , scope :: !Text
  } deriving (Eq, Show)

instance FromJSON IncludeDirectory where
  parseJSON (String s) = pure (IncludeDirectory s "public")
  parseJSON (Object v) =
    IncludeDirectory <$> v .:  "path"
                     <*> v .:? "scope" .!= "public"
  parseJSON _ = fail "‘includeDirectories’ list entries must be strings or objects"

includeDirectories :: Object -> Parser [IncludeDirectory]
includeDirectories v = do
  orign <- v .:? "includeDirectories"
  alias <- v .:? "includeDirs"
  pure (orign <|> alias) .!= []
