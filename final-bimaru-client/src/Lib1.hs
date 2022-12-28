{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State(..), State, emptyState, gameStart, render, mkCheck, toggle,
) where

import Types

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State{
    rows :: [Int],
    cols :: [Int],
    toggled :: [Coord]
} deriving (Eq, Show)


-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State [] [] []

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart (State _ _ d) f = State y z d
    where
        y = makeList(findSubstring "occupied_rows" (toMap f))
        z = makeList(findSubstring "occupied_cols" (toMap f))


toMap :: Document -> [(String, Document)]
toMap (DMap a) = a
toMap _ = []

toNum' :: Document -> Int
toNum' (DInteger a) = a
toNum' _ = -1

toList' :: Document -> [Document]
toList' (DList a) = a
toList' _ = []

makeList :: Document -> [Int]
makeList (DMap (x:xs)) = toNum' (snd x) : getTail xs
    where
        getTail :: [(String, Document)] -> [Int]
        getTail (a:_) = getInteger (snd a)
            where
                getInteger :: Document -> [Int]
                getInteger DNull = []
                getInteger (DMap (z:zs)) = toNum' (snd z) : getTail zs
                getInteger _ = error "Error when making list"
        getTail _ = error "Error when getting tail"
makeList _ = error "Here"


findSubstring :: String -> [(String, Document)] -> Document
findSubstring key [] = error (show key ++ "Key not found")
findSubstring key a = 
    if key == fst (head a)
    then snd (head a)
    else findSubstring key (tail a)
        

-- IMPLEMENT
-- renders your game board
render :: State -> String
render = show
-- render (State _ b c d) = firstSpaces c b d
--     where
--         --             Rows     Cols     Toggled
--         firstSpaces :: [Int] -> [Int] -> [Coord] -> String
--         firstSpaces b' c' d' = "RUSKI VOIENY KARABL - IDI NX\n   " ++ headerRow b' c' d'
--             where
--                 --          Rows     Cols     Toggled
--                 headerRow :: [Int] -> [Int] -> [Coord] -> String
--                 headerRow (x:xs) y z = show x ++ " " ++  headerRow xs y z
--                 headerRow [] y z= filter (\xs -> (xs /='"')) ("\n" ++ otherRows y 0 z "")
--                     where
--                         --           Cols    Row nr. Toggled   Visi likę rows
--                         otherRows :: [Int] -> Int -> [Coord] -> String -> String
--                         otherRows [] 10 _ e = e
--                         otherRows (x:xs) y' z' e = e ++ otherRows xs (y'+1) z (show x ++ " " ++ fillUpWithData 9 y' z' ++ " \n")
--                             where
--                                 --              rows left Col nr.  Toggled
--                                 fillUpWithData :: Int -> Int -> [Coord] -> String
--                                 fillUpWithData (-1) _ _= []
--                                 fillUpWithData a b'' c'' =
--                                     if elem (Coord a b'') c''
--                                     then fillUpWithData (a-1) b'' c'' ++ " #"
--                                     else fillUpWithData (a-1) b'' c'' ++ " _"
--                         otherRows _ _ _ _= error "otherRows"


-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck (State _ _ x) = Check x


-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State b c d) x =
    if odd (length x)
    then error "Not enough arguments"
    else saveInput (State b c d) x
        where
            saveInput :: State -> [String] -> State
            saveInput (State b' c' d') x' = State b' c' (createCoord x' d')
                where
                    createCoord :: [String] -> [Coord] -> [Coord]
                    createCoord [] x'' = x''
                    createCoord (x'':y:xs) d'' =  checkToggled xs (Coord (read x'')(read y)) d''
                        where 
                            checkToggled :: [String] -> Coord -> [Coord] -> [Coord]
                            checkToggled s q w =
                                if elem q w
                                then createCoord s (removeOne w q)
                                else createCoord s (q : w)
                                    where
                                        removeOne :: [Coord] -> Coord -> [Coord]
                                        removeOne = \list v -> 
                                            case list of 
                                            [] -> error "Element not found!"
                                            e:es | v==e -> es
                                            e:es -> e : removeOne es v
                    createCoord _ _ = error "createCoord"

-- IMPLEMENT
-- Adds hint data to the game state
-- hint :: State -> Document -> State
-- hint (State b c d) t = State b c x
--     where
--         x = fillUpHints (toMap t) ++ d


fillUpHints :: [(String, Document)] -> [Coord]
fillUpHints (x:_) = getElements (toList' $ snd x)
    where
        getElements :: [Document] -> [Coord]
        getElements [] = []
        getElements (y:ys) = go (toMap y) : getElements ys
            where
                go :: [(String, Document)] -> Coord
                go (x':y':[]) = Coord (toNum' $ snd x') (toNum' $ snd y')
                go _ = error "go"
fillUpHints _ = error "Filling up"