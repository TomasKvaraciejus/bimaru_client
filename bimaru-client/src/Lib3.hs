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
parseDocument str = createDocument str

createType :: String -> Either String Document -- creates base type (DString, DInteger, DNull)
createType str
    | readLine str == "null" || readLine str == "~" = Right DNull
    | isInteger (removeAllSpacing (readLine str)) False = Right $ DInteger $ read $ readLine str
    | otherwise = Right $ DString $ readLine str

createDocument :: String -> Either String Document 
createDocument str
    | elem '-' (readLine str) = do
        DList <$> (createDList str (spacingLength str))
    | elem ':' (readLine str) = do
        a <- createDMap str (spacingLengthDMap str)
        return $ DMap a
    | otherwise = createType str

createDMap :: String -> Int -> Either String [(String, Document)] -- recursively creates DMap
createDMap [] n = Right []
createDMap str n =
    do
        a <- parseDMap str -- a = (key, value)
        b <- if str == [] then Right [] else if take 1 (drop (getLengthUntil str ':' + 1) str) /= "\n" -- b = recursively created DMap to append take 1 (drop (getLengthUntil str ':' + 1) str)
                                        then createDMap ((dropUntilSameSpacing (dropUntil str '\n') n) True) n -- recursively call next line
                                        else createDMap (dropUntilSameSpacing (dropUntil (dropUntil str '\n') '\n') n True) n -- recursively call next next line
        return $ a : b

createDList :: String -> Int -> Either String [Document]
createDList [] n = Right []
createDList str n =
    do
        a <- createDListElement $ str
        b <- if str == [] then Right [] else createDList ((dropUntilSameSpacing (dropUntil str '\n') n) False) n
        return $ a : b

createDListElement :: String -> Either String Document
createDListElement str
    | elem ':' (readLine str) = do
        a <- createDMap str (spacingLengthDMap str)
        return $ DMap a
    | otherwise = createType $ removeAllSpacing $ readLine str

isDNull :: Document -> Bool
isDNull DNull = True
isDNull _ = False

isValueOnSameLine :: String -> Bool
isValueOnSameLine str = take 1 (drop (getLengthUntil str ':' + 1) str) /= "\n"

parseDMap :: String -> Either String (String, Document)
parseDMap str = do
    key <- checkDMapKey $ removeAllSpacing str
    value       <- if take 1 (drop (getLengthUntil str ':' + 1) str) /= "\n"
                   then createType $ (dropUntil (readLine str) ':') --createDocument $ drop 1 (dropUntil str ':') 
                   else createDocument (dropUntil str '\n')
    return $ (key, value)

parseDMapValue :: String -> Either String Document
parseDMapValue str
    | length(filter (`elem` nums) str) == length str = Right $ DInteger $ readInt str
    | take 1 str == "-" && length(filter (`elem` nums) (drop 1 str)) == length str - 1 = Right $ DInteger $ readInt str
    | str == "null" = Right DNull
    | otherwise = Right $ DString str

parseDash :: String -> Either String((), String)
parseDash ('-':x) = Right ((), x)
parseDash _ = Left "Dash expected"

parseChar :: Char -> String -> Either String (Char, String)
parseChar ch [] = Left $ "Empty input: '" ++ [ch] ++ "' expected"
parseChar ch (x:xs) | ch == x = Right (x, xs)
                    | otherwise = Left $ ch :" expected"

errorMsg :: String -> String
errorMsg x = "In expression -> " ++ if x == "" then "\'\'" else x

dropUntil :: String -> Char -> String
dropUntil s c = drop (getLengthUntil s c + 1) s

dropUntilSameSpacing :: String -> Int -> Bool -> String
dropUntilSameSpacing [] _ _ = []
dropUntilSameSpacing str n dmap 
    | dmap = if spacingLength str == n then str 
             else if spacingLengthDMap str == n then [] else dropUntilSameSpacing (dropUntil str '\n') n dmap
    | otherwise = if spacingLength str == n then str else dropUntilSameSpacing (dropUntil str '\n') n dmap

readLine :: String -> String
readLine str = take (getLengthUntil str '\n') str

getNextLine :: String -> String
getNextLine [] = []
getNextLine str = let x = (drop (getLengthUntil str '\n' + 1) str) in take (getLengthUntil x '\n' + 1) x

getLengthUntil :: String -> Char -> Int
getLengthUntil [] _ = 0
getLengthUntil (s:xs) c
    | s == c = getLengthUntil [] c
    | otherwise = getLengthUntil xs c + 1

isInteger :: String -> Bool -> Bool
isInteger [] b = b
isInteger (s:xs) b
    | elem s ['0'..'9'] = isInteger xs True
    | otherwise = isInteger [] False

removeAllSpacing :: String -> String
removeAllSpacing [] = []
removeAllSpacing (s:xs)
    | s == ' ' || s == '-' = removeAllSpacing xs
    | otherwise = [s] ++ removeAllSpacing xs

spacingLength :: String -> Int
spacingLength [] = 0
spacingLength (s:xs)
    | s == ' ' = spacingLength xs + 1
    | s == '\n' || s == ':' = spacingLength []
    | otherwise = spacingLength xs

spacingLengthDMap :: String -> Int
spacingLengthDMap [] = 0
spacingLengthDMap (s:xs)
    | s == ' ' || s == '-' = spacingLengthDMap xs + 1
    | s == '\n' || s == ':' = spacingLengthDMap []
    | otherwise = spacingLengthDMap xs

getHeights :: String -> [Int]
getHeights [] = []
getHeights s = [(spacingLength (readLine s))] ++ getHeights (dropUntil s '\n')

letters :: String
letters = ['A'..'Z'] ++ ['a'..'z']

alphaNums :: String
alphaNums = letters ++ ['0'..'9']

nums :: String
nums = ['0'..'9']

readInt :: String -> Int
readInt = read

checkDMapKey :: String -> Either String String
checkDMapKey x =
    let untilColon = takeWhile (/= ':' ) x
    in
        --if length (filter (`elem` letters) untilColon) /= length untilColon then Left $ "DMap key contained illegal characters. " ++ errorMsg x else
        if null untilColon then Left $ "No DMap key is present. " ++ errorMsg x else
        if length untilColon > 10 then Left $ "DMap key was too long. " ++ errorMsg x
        -- if length untilColon == length x then Left $ "No \':\' after DMap key. " ++ errorMsg x else
        --if take 1 (drop (length untilColon) x) /= ":" then Left $ "No \':\' after DMap key. " ++ errorMsg x
        else Right untilColon

checkDMapValue :: String -> String -> Either String String
checkDMapValue key x =
    let value = take 1 (drop 1 x)
        rest = drop 1 x
    in
        if take 1 x /= " " then Left $ "No space after DMap key's colon. " ++ errorMsg key ++ x else
        if value == " " then Left $ "Only one space after DMaps's key is allowed. " ++ errorMsg key ++ x else
        if length (filter (`notElem` alphaNums ++ "-") rest) /= 0 then Left $ "DMap's value contains illegal characters. " ++ errorMsg key ++ x
        else Right rest

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
