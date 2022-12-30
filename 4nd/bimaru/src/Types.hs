{-# LANGUAGE DeriveGeneric #-}
module Types (
    Document(..), Check(..), Coord(..), State(..),
    ToDocument, toDocument, FDocument, fDocument,
    FromDocument, fromDocument
) where

import Data.Map as Map ()
import Data.Aeson as A
import Data.Yaml as Y ()
import Data.HashMap.Strict as HMS ()
import Data.Text as T ()
import Data.Vector as V ()
import qualified Data.List as L
import Data.Scientific as S ()
import GHC.Generics
import Data.String.Conversions ()
import Control.Monad.Trans.Except
import Control.Monad.Except ()

newtype Check = Check {
    coords :: [Coord]
} deriving (Generic, Show, Eq)

instance ToJSON Check

data Coord = Coord {
    col :: Int,
    row :: Int
} deriving (Generic, Show, Eq)
instance ToJSON Coord

data Document =
    DMap [(String, Document)]
    | DList [Document]
    | DInteger Int
    | DString String
    | DNull
    deriving (Show, Ord)

data State = State{
    rows :: [Int],
    cols :: [Int],
    toggled :: [Coord]
} deriving (Eq, Show)

instance Eq Document where
    DNull == DNull = True
    DString s1 == DString s2 = s1 == s2
    DInteger i1 == DInteger i2 = i1 == i2
    DList l1 == DList l2 = l1 == l2
    DMap kv1 == DMap kv2 = L.sort kv1 == L.sort kv2
    _ == _ = False


class ToDocument a where
    toDocument :: a -> Document

class FromDocument a where
    fromDocument :: Document -> ExceptT String IO a

class FDocument a where
    fDocument :: a -> ExceptT String IO [Coord]