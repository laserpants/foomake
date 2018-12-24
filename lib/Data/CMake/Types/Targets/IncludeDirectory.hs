{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.IncludeDirectory
  ( IncludeDirectory(..)
  , Scope(..)
  , includeDirectories
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.CMake.Utils
import Data.Maybe (maybeToList)
import Data.Text

data Scope = Public | Private | Interface | Unspecified
  deriving (Eq, Show)

data IncludeDirectory = IncludeDirectory
  { path  :: !Text
  , scope :: !Scope
  } deriving (Eq, Show)

instance FromJSON IncludeDirectory where
  parseJSON (String s) = pure (IncludeDirectory s Unspecified)
  parseJSON (Object v) =
    IncludeDirectory <$> v .: "path"
                     <*> pure Unspecified
  parseJSON _ = fail "‘includeDirectories’ list entries must be strings or objects"

data IncludeGroup = IncludeGroup ![IncludeDirectory]
                                 ![IncludeDirectory]
                                 ![IncludeDirectory]

instance FromJSON IncludeGroup where
  parseJSON (Object v) =
    IncludeGroup <$> v .:? "public"    .!= []
                 <*> v .:? "private"   .!= []
                 <*> v .:? "interface" .!= []
  parseJSON _ = fail "‘includeDirectories’ must be an object"

setScope :: Scope -> [IncludeDirectory] -> [IncludeDirectory]
setScope newScope = fmap (\dir -> dir{ scope = newScope })

extractDirs :: Maybe IncludeGroup -> [IncludeDirectory]
extractDirs Nothing      = []
extractDirs (Just group) = setScope Public public
                        ++ setScope Private private
                        ++ setScope Interface interface
  where
    IncludeGroup public private interface = group

prop :: FromJSON a => Object -> Parser (Maybe a)
prop v = parseAlias v "includeDirectories" "includeDirs"

includeDirectories :: Object -> Parser [IncludeDirectory]
includeDirectories v = dict <|> list where
    dict = liftM extractDirs (prop v)
    list = liftM (join . maybeToList) (prop v)
