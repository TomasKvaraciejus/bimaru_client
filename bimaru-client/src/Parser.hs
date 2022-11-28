{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Parser () where

data Document =
    DMap [(String, Document)]
    | DList [Document]
    | DInteger Int
    | DString String
    | DNull
    deriving (Show, Eq, Ord)

parseDocument :: String -> Either String Document
parseDocument x 
    | elem ':' x = parseDMap x
    | elem '-' x = parseDList x
    | otherwise = Left "AAA"

parseValue :: String -> Document
parseValue x = DString "AAA"
    -- | elem ['0'..'9'] x = error "aaa"

parseDList :: String -> Either String Document
parseDList str = Left "AAA"

parseDMap :: String -> Either String Document
parseDMap str = do
    (key, tail) <- checkDMapKey str
    value       <- checkDMapValue key tail
    doc         <- parseDMapValue value
    return $ DMap [(key, doc)]

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

dropLine :: String -> String
dropLine s = drop (getLengthUntil s '\n' + 1) s

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

spacingLength :: String -> Int
spacingLength [] = 0
spacingLength (s:xs) 
    | s == ' ' = spacingLength xs + 1
    | otherwise = spacingLength xs

letters :: String
letters = ['A'..'Z'] ++ ['a'..'z']

alphaNums :: String
alphaNums = letters ++ ['1'..'9']

nums :: String
nums = ['1'..'9']

readInt :: String -> Int
readInt = read

checkDMapKey :: String -> Either String (String, String)
checkDMapKey x = 
    let untilColon = takeWhile (/= ':' ) x
    in
        if length (filter (`elem` letters) untilColon) /= length untilColon then Left $ "DMap key contained illegal characters. " ++ errorMsg x else
        if null untilColon then Left $ "No DMap key is present. " ++ errorMsg x else 
        if length untilColon > 10 then Left $ "DMap key was too long. " ++ errorMsg x else
        if length untilColon == length x then Left $ "No \':\' after DMap key. " ++ errorMsg x else
        if take 1 (drop (length untilColon) x) /= ":" then Left $ "No \':\' after DMap key. " ++ errorMsg x
        else Right (untilColon, drop (length untilColon + 1) x)

checkDMapValue :: String -> String -> Either String String
checkDMapValue key x =
    let value = take 1 (drop 1 x)
        rest = drop 1 x
    in
        if take 1 x /= " " then Left $ "No space after DMap key's colon. " ++ errorMsg key ++ x else
        if value == " " then Left $ "Only one space after DMaps's key is allowed. " ++ errorMsg key ++ x else
        if length (filter (`notElem` alphaNums ++ "-") rest) /= 0 then Left $ "DMap's value contains illegal characters. " ++ errorMsg key ++ x
        else Right rest