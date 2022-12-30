{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib where

import Types(State(..), Document(..), Coord(..), Check(..))
import Util
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy as S (State, get)

emptyState :: Types.State
emptyState = State [] [] []



data GameStart = GameStart{
  gameId :: Int,
  rows :: [Int],
  cols :: [Int]
} deriving Show

gameStart :: Types.State -> GameStart -> Types.State
gameStart (State _ _ d) (GameStart _ f g) = State f g d

----

toNum' :: Document -> Int
toNum' (DInteger a) = a
toNum' _ = -1

toMap :: Document -> [(String, Document)]
toMap (DMap a) = a
toMap _ = []

getNum :: Either String Document -> ExceptT String IO Int
getNum (Right (DInteger a)) = return a
getNum _ = throwE "not an int"

findSubstring :: String -> [(String, Document)] -> Either String Document
findSubstring key [] = Left (key ++ " key not found or is placed incorrectly")
findSubstring key a =
    if key == fst (head a)
    then Right $ snd (head a)
    else findSubstring key (tail a)
----


makeList :: Either String Document -> Either String [Int]
makeList (Right (DMap xs)) = parseFurther xs
makeList _ = Left "improper type when creating a list"

parseFurther :: [(String, Document)] -> Either String [Int]
parseFurther (x:xs) = do
    let first = getTail xs
    case first of
        Right a -> Right $ toNum' (snd x) : a
        Left b -> Left b
parseFurther _ = Left "Unexpected error in parseFurther"

getTail :: [(String, Document)] -> Either String [Int]
getTail (a:_) =  do
    let second = getInteger (snd a)
    case second of
        Right a' -> Right a'
        Left b' -> Left b'
getTail _ = Left "Error in getTail"


getInteger :: Document -> Either String [Int]
getInteger DNull = Right []
getInteger (DMap (x:xs)) = do
    let third = getTail xs
    case third of
        Right a -> return $ toNum' (snd x) : a
        Left b -> Left b
getInteger _ = Left "Error in getInteger"


render :: Types.State -> String
-- render = show
render (State b c d) = firstSpaces c b d
    where
        --             Rows     Cols     Toggled
        firstSpaces :: [Int] -> [Int] -> [Coord] -> String
        firstSpaces b' c' d' = "RUSKI VOIENY KARABL - IDI NX\n     0 1 2 3 4 5 6 7 8 9\n     " ++ headerRow b' c' d'
            where
                --          Rows     Cols     Toggled
                headerRow :: [Int] -> [Int] -> [Coord] -> String
                headerRow (x:xs) y z = show x ++ " " ++  headerRow xs y z
                headerRow [] y z= filter (\xs -> (xs /='"')) ("\n" ++ otherRows y 0 z "")
                    where
                        --           Cols    Row nr. Toggled   Visi likę rows
                        otherRows :: [Int] -> Int -> [Coord] -> String -> String
                        otherRows [] 10 _ e = e
                        otherRows (x:xs) y' z' e = e ++ otherRows xs (y'+ 1) z (show y' ++ " " ++ show x ++ " " ++ fillUpWithData 9 y' z' ++ " \n")
                            where
                                --              rows left Col nr.  Toggled
                                fillUpWithData :: Int -> Int -> [Coord] -> String
                                fillUpWithData (-1) _ _= []
                                fillUpWithData a b'' c'' =
                                    if elem (Coord a b'') c''
                                    then fillUpWithData (a-1) b'' c'' ++ " #"
                                    else fillUpWithData (a-1) b'' c'' ++ " _"
                        otherRows _ _ _ _= error "otherRows"


-- IMPLEMENT
-- Make check from current state
mkCheck :: Types.State -> Check
mkCheck (State _ _ x) = Check x


-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: Types.State -> [String] -> Types.State
toggle (State b c d) x =
    if odd (length x)
    then error "Not enough arguments"
    else saveInput (State b c d) x
        where
            saveInput :: Types.State -> [String] -> Types.State
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