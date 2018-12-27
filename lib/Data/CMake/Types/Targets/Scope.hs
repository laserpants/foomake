{-# LANGUAGE OverloadedStrings #-}
module Data.CMake.Types.Targets.Scope
  ( Scope(..)
  , ScopeGroup(..)
  , HasScope(..)
  , ungroup
  ) where

import Data.Aeson
import Data.Text

data Scope = Public | Private | Interface | Unspecified
  deriving (Eq, Show)

data ScopeGroup a = ScopeGroup ![a] ![a] ![a] 

instance FromJSON a => FromJSON (ScopeGroup a) where
  parseJSON (Object v) =
    ScopeGroup <$> v .:? "public"    .!= []
               <*> v .:? "private"   .!= []
               <*> v .:? "interface" .!= []
  parseJSON _ = fail ""

class HasScope a where 
  setScope :: Scope -> a -> a

ungroup :: HasScope a => ScopeGroup a -> [a]
ungroup group = fmap (setScope Public) public
             ++ fmap (setScope Private) private
             ++ fmap (setScope Interface) interface
  where ScopeGroup public private interface = group

