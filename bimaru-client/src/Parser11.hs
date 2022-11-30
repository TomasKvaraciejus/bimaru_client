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
parseDocument str = createDocument str

createType :: String -> Either String Document -- creates base type (DString, DInteger, DNull)
createType str
    | removeForwardSpacing (readLine str) == "null" || removeForwardSpacing (readLine str) == "~" || removeForwardSpacing (readLine str) == "" = Right DNull
    | isInteger (removeForwardSpacing (removeBackSpacing (readLine str))) False = parseDInteger str
    | otherwise = Right $ DString $ removeForwardSpacing $ readLine str


parseDInteger :: String -> Either String Document
parseDInteger str
    | elem '-' str =
        if not (isInteger (dropUntil str '-') False) then Left "incorrect integer format"
        else Right $ DInteger $ read (drop (getLengthUntil str '-') str)
    | otherwise = Right $ DInteger $ read str

createDocument :: String -> Either String Document
createDocument str
    | elem '-' (readLine str) = do
        DList <$> (createDList str (spacingLength str))
    | elem ':' (readLine str) = do
        a <- createDMap str (spacingLengthDMap str)
        return $ DMap a
    | otherwise = createType $ removeBackSpacing str

createDMap :: String -> Int -> Either String [(String, Document)] -- recursively creates DMap
createDMap [] n = Right []
createDMap str n =
    do
        a <- parseDMap str -- a = (key, value)
        b <- if str == [] then Right [] else 
            do
            s <- if isValueOnSameLine str
                 then ((dropUntilSameSpacing (dropUntil str '\n') n) True)
                 else 
                    if isEmptyLine (getNextLine str) then (dropUntilSameSpacing (dropUntil (dropUntil str '\n') '\n') n True)
                    else ((dropUntilSameSpacing (dropUntil str '\n') n) True)
            createDMap s n-- b = recursively created DMap to append take 1 (drop (getLengthUntil str ':' + 1) str)
        return $ a : b

createDList :: String -> Int -> Either String [Document]
createDList [] n = Right []
createDList str n =
    do
        a <- createDListElement $ str
        b <- if str == [] then Right [] else
            do
                a <- dropUntilSameSpacing (dropUntil str '\n') n False
                createDList a n
        return $ a : b

createDListElement :: String -> Either String Document
createDListElement str
    | elem ':' (readLine str) = do
        a <- createDMap str (spacingLengthDMap str)
        return $ DMap a
    | otherwise = createType $ removeBackSpacing $ readLine str

isDNull :: Document -> Bool
isDNull DNull = True
isDNull _ = False

isValueOnSameLine :: String -> Bool
isValueOnSameLine str = (dropWhile (\x -> x == ' ') (dropUntil (readLine str) ':')) /= []

parseDMap :: String -> Either String (String, Document)
parseDMap str = do
    key <- checkDMapKey $ removeBackSpacing str
    value       <- if isValueOnSameLine str -- take 1 (drop (getLengthUntil str ':' + 1) str)
                   then createType $ removeBackSpacing (dropUntil (readLine str) ':')
                   else if elem ':' (getNextLine str) && notElem '-' (getNextLine str)
                        then Right DNull
                        else if spacingLength str == spacingLength (getNextLine str) && spacingLength str /= 0 
                             then Left "DMap value on same height as key" 
                             else createDocument (dropUntil str '\n')
    return (key, value)

-- parseDMapValue :: String -> Either String Document
-- parseDMapValue str
--     | length(filter (`elem` nums) str) == length str = Right $ DInteger $ readInt str
--     | take 1 str == "-" && length(filter (`elem` nums) (drop 1 str)) == length str - 1 = Right $ DInteger $ readInt str
--     | str == "null" = Right DNull
--     | otherwise = Right $ DString str

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

dropUntilSameSpacing :: String -> Int -> Bool -> Either String String
dropUntilSameSpacing [] _ _ = Right []
dropUntilSameSpacing str n dmap
    | dmap = if spacingLength str == n then Right str
             else if spacingLengthDMap str == n
                then Right []
                else if elem ':' str then dropUntilSameSpacing (dropUntil str '\n') n dmap
                else Left "incorrect height formatting"
    | otherwise = if spacingLength str == n then Right str else dropUntilSameSpacing (dropUntil str '\n') n dmap

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

isEmptyLine :: String -> Bool
isEmptyLine str = length (dropWhile (\x -> x == ' ') str) == 0

-- removeBackSpacing :: String -> String
-- removeBackSpacing [] = []
-- removeBackSpacing (s:xs)
--     | s == ' ' || s == '-' = removeBackSpacing xs
--     | otherwise = [s] ++ removeBackSpacing xs

removeBackSpacing :: String -> String
removeBackSpacing str = let x = dropWhile (\x -> x == ' ') (str) in if head x == '-' then drop 2 x else x

removeForwardSpacing:: String -> String
removeForwardSpacing str = reverse (dropWhile (\x -> not $ elem x (alphaNums ++ ['-'])) (dropWhile (\x -> not $ elem x (alphaNums ++ ['-'])) (reverse (readLine str))))

spacingLength :: String -> Int
spacingLength [] = 0
spacingLength (s:xs)
    | s == ' ' = spacingLength xs + 1
    | s == '\n' || s == ':' || elem s alphaNums = spacingLength []
    | otherwise = spacingLength xs

spacingLengthDMap :: String -> Int
spacingLengthDMap [] = 0
spacingLengthDMap (s:xs)
    | s == ' ' || s == '-' = spacingLengthDMap xs + 1
    | s == '\n' || s == ':' || elem s alphaNums = spacingLengthDMap []
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
        else Right $ removeForwardSpacing untilColon

checkDMapValue :: String -> String -> Either String String
checkDMapValue key x =
    let value = take 1 (drop 1 x)
        rest = drop 1 x
    in
        if take 1 x /= " " then Left $ "No space after DMap key's colon. " ++ errorMsg key ++ x else
        if value == " " then Left $ "Only one space after DMaps's key is allowed. " ++ errorMsg key ++ x else
        if length (filter (`notElem` alphaNums ++ "-") rest) /= 0 then Left $ "DMap's value contains illegal characters. " ++ errorMsg key ++ x
        else Right rest