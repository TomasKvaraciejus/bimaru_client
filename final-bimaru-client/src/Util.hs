module Util where

import Control.Monad.Trans.Except
import Control.Monad.State.Lazy

readLine :: String -> String
readLine str = take (getLengthUntil str '\n') str

spacingLength :: String -> Int
spacingLength [] = 0
spacingLength (s:xs)
    | s == ' ' = spacingLength xs + 1
    | s == '\n' || s == '-' || elem s alphaNums = spacingLength []
    | otherwise = spacingLength xs

dropUntil :: String -> Char -> String
dropUntil s c = drop (getLengthUntil s c + 1) s

getLengthUntil :: String -> Char -> Int
getLengthUntil [] _ = 0
getLengthUntil (s:xs) c
    | s == c = getLengthUntil [] c
    | otherwise = getLengthUntil xs c + 1

removeBackSpacing :: String -> String
removeBackSpacing str = let x = dropWhile (== ' ') str in if take 1 x == "-" then drop 1 x else x

removeBackSpacingDMap :: String -> String
removeBackSpacingDMap = dropWhile (== ' ')

removeForwardSpacing:: String -> String
removeForwardSpacing str = reverse (dropWhile (\x -> not $ elem x (alphaNums ++ ['-'])) (dropWhile (\x -> not $ elem x (alphaNums ++ ['-'])) (reverse (readLine str))))

getQuotedValue :: String -> String
getQuotedValue str = (takeWhile (\x -> x /= '\'' && x /= '\"') (drop 1 (dropWhile (\x -> x /= '\'' && x /= '\"')  str)))

errorMsg :: String -> String
errorMsg x = "In expression -> " ++ if x == "" then "\'\'" else x

isInteger :: String -> Bool -> Bool
isInteger [] b = b
isInteger (s:xs) _
    | elem s ['0'..'9'] = isInteger xs True
    | otherwise = isInteger [] False

checkType :: String -> Bool
checkType str = if (getNextLine str) == [] then True 
                else if ((elem ':' (getNextLine str)) || (elem '-' (getNextLine str))) then True
                else False

getNextLine :: String -> String
getNextLine [] = []
getNextLine str = let x = (drop (getLengthUntil str '\n' + 1) str) in take (getLengthUntil x '\n' + 1) x

replaceDash :: String -> String
replaceDash str
    | elem '-' (readLine str) = createSpacing (getLengthUntil str '-' + 1) ++ (drop (getLengthUntil str '-' + 1) str)
    | otherwise = str

createSpacing :: Int -> String
createSpacing 0 = []
createSpacing n = [' '] ++ createSpacing (n - 1)

dropUntilSameSpacing :: String -> Int -> Bool -> ExceptT String (State String) String
dropUntilSameSpacing [] _ _ = return []
dropUntilSameSpacing str n dmap
    | dmap = if (spacingLength (str)) == n && (notElem '-' (readLine str) || (take 1 (dropUntil str '-')) /= " ") then 
                if elem ':' (readLine str) 
                then return str
                else throwE $ "incorrect height formatting. " ++ errorMsg str
             else 
                if spacingLength (str) < n then return []
                else dropUntilSameSpacing (dropUntil str '\n') n dmap
    | otherwise = if (spacingLength (str)) == n
                  then if ((elem '-' (readLine str)) && take 1 (dropUntil str '-') == " ") 
                       then return str 
                       else if (elem ':' (readLine str)) && (not $ isValueOnSameLine str)
                            then return []
                            else dropUntilSameSpacing (dropUntil str '\n') n dmap
                  else 
                      if (spacingLength (str)) < n then return [] 
                      else dropUntilSameSpacing (dropUntil str '\n') n dmap

isValueOnSameLine :: String -> Bool
isValueOnSameLine str = (dropWhile (== ' ') (dropUntil (readLine str) ':')) /= []

containsQuotedValue :: String -> Bool
containsQuotedValue str = (elem '\'' str) || (elem '\"' str)

checkDMapKey :: String -> ExceptT String (State String) String
checkDMapKey x =
    let untilColon = if containsQuotedValue (takeWhile (/= ':' ) x) 
                     then getQuotedValue (takeWhile (/= ':' ) x) 
                     else removeForwardSpacing $ removeBackSpacingDMap (takeWhile (/= ':' ) x)
    in
        if null untilColon then throwE $ "No DMap key is present. " ++ errorMsg x else
        if take 1 (dropUntil (readLine x) ':') /= " " && take 1 (dropUntil (readLine x) ':') /= [] then throwE $ "No space after DMap key. " ++ errorMsg x
        else return untilColon

checkDocument :: String -> Int -> Bool
checkDocument [] _ = True
checkDocument str n
    | spacingLength str < n = False
    | otherwise = checkDocument (dropUntil str '\n') n

letters :: String
letters = ['A'..'Z'] ++ ['a'..'z']

alphaNums :: String
alphaNums = letters ++ ['0'..'9']