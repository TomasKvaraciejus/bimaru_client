{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types
import Lib1 (State(..))
import Lib2 (isDocumentCorrect, wrongState, keysFound, notFoundKey, checkNumHints, findSubstring, toMap, makeList, findSubstring, checkHintLength, fillUpHints)

parseDocument :: String -> Either String Document
parseDocument str = if take 1 (reverse str) == "\n" 
                    then if checkDocument str (spacingLength str) 
                         then createDocument str 
                         else Left $ "incorrect document format"
                    else Left $ "missing endl at end"

createType :: String -> Either String Document -- creates base type (DString, DInteger, DNull)
createType str
    | elem '\'' (readLine str) || elem '\"' (readLine str) = Right $ DString $ getQuotedValue str 
    | removeForwardSpacing (removeBackSpacing $ readLine str) == "null" || removeForwardSpacing (removeBackSpacing $ readLine str) == "~" = Right DNull
    | isInteger (removeForwardSpacing (removeBackSpacing (readLine str))) False = parseDInteger $ readLine str
    | otherwise = Right $ DString $ removeForwardSpacing $ removeBackSpacingDMap (readLine str)

checkDocument :: String -> Int -> Bool
checkDocument [] _ = True
checkDocument str n
    | spacingLength str < n = False
    | otherwise = checkDocument (dropUntil str '\n') n

checkType :: String -> Bool
checkType str = if (getNextLine str) == [] then True 
                else if ((elem ':' (getNextLine str)) || (elem '-' (getNextLine str))) then True
                else False

parseDInteger :: String -> Either String Document
parseDInteger str
    | elem '-' str =
        if not (isInteger (dropUntil str '-') False) then Left $ "incorrect integer format. " ++ errorMsg str
        else Right $ DInteger $ read (drop (getLengthUntil str '-') str)
    | otherwise = Right $ DInteger $ read str

createDocument :: String -> Either String Document
createDocument str
    | removeBackSpacingDMap (readLine str) == "[]" = Right $ DList []
    | removeBackSpacingDMap (readLine str) == "{}" = Right $ DMap []
    | elem '-' (readLine str) && (take 1 (dropUntil str '-') == " ") = do
        DList <$> ((createDList str (spacingLength str)))
    | elem ':' (readLine str) = do
        a <- createDMap str (spacingLength str)
        return $ DMap a
    | otherwise =   if checkType str 
                    then createType $ str
                    else Left "AAA"

createDMap :: String -> Int -> Either String [(String, Document)] -- recursively creates DMap
createDMap [] _ = Right []
createDMap str n =
    do
        a <- parseDMap str -- a = (key, value)
        b <- if str == [] then Right [] else 
            do
            s <- dropUntilSameSpacing (dropUntil str '\n') n True -- if isValueOnSameLine str
            createDMap s n -- b = recursively created DMap to append take 1 (drop (getLengthUntil str ':' + 1) str)
        return $ a : b

parseDMap :: String -> Either String (String, Document)
parseDMap str = do
    key <- checkDMapKey $ readLine str
    value       <- parseDMapValue str
    return (key, value)

parseDMapValue :: String -> Either String Document
parseDMapValue str 
    | isValueOnSameLine str = createDocument $ (dropUntil (readLine str) ':')
    | otherwise = if spacingLength (getNextLine str) >= spacingLength str
                  then 
                    if (spacingLength str > (spacingLength (getNextLine str))) && (elem ':' (getNextLine str)) 
                    then Right DNull
                    else createDocument $ dropUntil str '\n'
                  else 
                    Left $ "DMap value on same height as key. " ++ errorMsg str

createDList :: String -> Int -> Either String [Document]
createDList [] _ = Right []
createDList str n =
    do
        a <- createDListElement $ replaceDash str
        b <- if str == [] then Right [] else
            do
                c <- dropUntilSameSpacing (dropUntil str '\n') n False
                createDList c n
        return $ a : b

createDListElement :: String -> Either String Document
createDListElement str = createDocument $ str

isDNull :: Document -> Bool
isDNull DNull = True
isDNull _ = False

isValueOnSameLine :: String -> Bool
isValueOnSameLine str = (dropWhile (== ' ') (dropUntil (readLine str) ':')) /= []

containsQuotedValue :: String -> Bool
containsQuotedValue str = (elem '\'' str) || (elem '\"' str)

getQuotedValue :: String -> String
getQuotedValue str = (takeWhile (\x -> x /= '\'' && x /= '\"') (drop 1 (dropWhile (\x -> x /= '\'' && x /= '\"')  str)))


errorMsg :: String -> String
errorMsg x = "In expression -> " ++ if x == "" then "\'\'" else x

dropUntil :: String -> Char -> String
dropUntil s c = drop (getLengthUntil s c + 1) s

dropUntilSameSpacing :: String -> Int -> Bool -> Either String String
dropUntilSameSpacing [] _ _ = Right []
dropUntilSameSpacing str n dmap
    | dmap = if (spacingLength (str)) == n && (notElem '-' (readLine str) || (take 1 (dropUntil str '-')) /= " ") then 
                if elem ':' (readLine str) 
                then Right str
                else Left $ "incorrect height formatting. " ++ errorMsg str
             else 
                if spacingLength (str) < n then Right []
                else dropUntilSameSpacing (dropUntil str '\n') n dmap

    | otherwise = if (spacingLength (str)) == n
                  then if ((elem '-' (readLine str)) && take 1 (dropUntil str '-') == " ") 
                       then Right str 
                       else if (elem ':' (readLine str)) && (not $ isValueOnSameLine str)
                            then Right []
                            else dropUntilSameSpacing (dropUntil str '\n') n dmap
                  else 
                      if (spacingLength (str)) < n then Right [] 
                      else dropUntilSameSpacing (dropUntil str '\n') n dmap


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
isInteger (s:xs) _
    | elem s ['0'..'9'] = isInteger xs True
    | otherwise = isInteger [] False

isEmptyLine :: String -> Bool
isEmptyLine str = length (dropWhile (\x -> x == ' ') str) == 0

replaceDash :: String -> String
replaceDash str
    | elem '-' (readLine str) = createSpacing (getLengthUntil str '-' + 1) ++ (drop (getLengthUntil str '-' + 1) str)
    | otherwise = str

createSpacing :: Int -> String
createSpacing 0 = []
createSpacing n = [' '] ++ createSpacing (n - 1)

removeBackSpacing :: String -> String
removeBackSpacing str = let x = dropWhile (== ' ') str in if take 1 x == "-" then drop 1 x else x

removeBackSpacingDMap :: String -> String
removeBackSpacingDMap = dropWhile (== ' ')

removeForwardSpacing:: String -> String
removeForwardSpacing str = reverse (dropWhile (\x -> not $ elem x (alphaNums ++ ['-'])) (dropWhile (\x -> not $ elem x (alphaNums ++ ['-'])) (reverse (readLine str))))

spacingLength :: String -> Int
spacingLength [] = 0
spacingLength (s:xs)
    | s == ' ' = spacingLength xs + 1
    | s == '\n' || s == '-' || elem s alphaNums = spacingLength []
    | otherwise = spacingLength xs

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
    let untilColon = if containsQuotedValue (takeWhile (/= ':' ) x) 
                     then getQuotedValue (takeWhile (/= ':' ) x) 
                     else removeForwardSpacing $ removeBackSpacingDMap (takeWhile (/= ':' ) x)
    in
        if null untilColon then Left $ "No DMap key is present. " ++ errorMsg x else
        if take 1 (dropUntil (readLine x) ':') /= " " && take 1 (dropUntil (readLine x) ':') /= [] then Left $ "No space after DMap key. " ++ errorMsg x
        else Right $ untilColon

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
instance FromDocument GameStart where 
  fromDocument e =
    if isDocumentCorrect e
    then moveWhenStateCorrect e
    else Left wrongState

data GameStart = GameStart{
  hints :: Int,
  rows :: [Int],
  cols :: [Int]
} deriving Show

moveWhenStateCorrect :: Document -> Either String GameStart
moveWhenStateCorrect a = do
    let keys = keysFound ["number_of_hints", "occupied_rows", "occupied_cols"] a
    case keys of
        Left str -> Left (str ++ notFoundKey)
        Right _ -> continueGameStart a
        
continueGameStart ::Document -> Either String GameStart
continueGameStart e = do
        x <- checkNumHints(findSubstring "number_of_hints" (toMap e))
        y <- makeList(findSubstring "occupied_rows" (toMap e))
        z <- makeList(findSubstring "occupied_cols" (toMap e))
        return (GameStart x y z )

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State _ _ _ d) (GameStart e f g) = State e f g d


-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
instance FromDocument Hint where 
  fromDocument t =
    if isDocumentCorrect t
    then checkFurther t
    else Left "Wrong State Hint data"

data Hint = Hint{
  toggled :: [Coord]
} deriving Show


checkFurther :: Document -> Either String Hint
checkFurther t = do
    let x = if isDocumentCorrect t then checkHintLength $ toMap t else Left "improper document type"
    case x of
        Right _ -> makeHint t
        Left y -> Left y


makeHint :: Document -> Either String Hint
makeHint t = do
    let x = fillUpHints (toMap t)
    case x of
        Right q -> return (Hint q)
        Left y -> Left y


-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State a b c d) (Hint e) = State a b c (e ++ d)

