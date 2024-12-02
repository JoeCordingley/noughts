{-# LANGUAGE DeriveGeneric #-}

module Json (Person (..), Move (..)) where

import Data.Aeson
import GHC.Generics

data Person = Person
    { name :: String
    , age :: Int
    }
    deriving (Generic, Show)

data Move = Nought | Cross String deriving (Generic, Show)

instance ToJSON Person
instance ToJSON Move
