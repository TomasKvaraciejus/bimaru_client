{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types
import Lib1 (State(..))

instance FromDocument GameStart where
    fromDocument (DString x) = Right (GameStart 0 [0] [0])
    fromDocument _ = Left "not a DString"

instance FromDocument Hint where
    fromDocument (DString x) = Right Hint
    fromDocument _ = Left "not a DString"

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument x = Right (DString x)

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart {
    a :: Int,
    b :: [Int],
    c :: [Int]
}deriving Show

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State a b c e) d = State a b c e

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint deriving Show

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State a b c e) h = State a b c e
