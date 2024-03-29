{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib2 (renderDocument, hint, gameStart, isDocumentCorrect, wrongState, keysFound, notFoundKey, checkNumHints, toMap, makeList, findSubstring, checkHintLength, fillUpHints) where
import Lib1 (State(..))
import Types
import Text.Read


wrongInput :: String
wrongInput = "Wrong input type. DList with atleast one DMap, containing 2 tuples expected"

wrongState :: String
wrongState = "Wrong State starting data"

incorrectDMap :: String
incorrectDMap = "Found incorrect DMap in DList"

notFoundKey :: String
notFoundKey =  " key not found or is placed incorrectly"

toNum :: Either String Document -> Either String Int
toNum (Right (DInteger a)) = Right a 
toNum _ = Left "Not a DInteger"

toMap :: Document -> [(String, Document)]
toMap (DMap a) = a
toMap _ = []

toNum' :: Document -> Int
toNum' (DInteger a) = a
toNum' _ = -1

toList' :: Document -> [Document]
toList' (DList a) = a
toList' _ = []

checkNumHints :: Either String Document -> Either String Int 
checkNumHints (Right (DInteger a)) = if a >= 0 then Right a else Left "All DIntegers must be [0,9]"
checkNumHints _ = Left "Not a DInteger"

-- IMPLEMENT
-- First, make Check an instance of ToDocument class

instance ToDocument Check where
    toDocument a = DMap[("coords", DList $ makeDList a)]
        where
            makeDList :: Check -> [Document]
            makeDList (Check []) = []
            makeDList (Check ((Coord x y):xs)) = DMap [("col", DInteger x), ("row", DInteger y)] : makeDList (Check xs)


-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument (DMap []) = "{}\n"
renderDocument (DList []) = "[]\n"
renderDocument (DMap a) = parseDMap 0 1 a
renderDocument (DList a) =  parseDList 0 1 a
renderDocument (DString a)  = parseDString a  ++ "\n"
renderDocument DNull = "null\n"
renderDocument (DInteger a) = show a ++ "\n"

parseDList :: Int -> Int -> [Document] -> String
parseDList _ _ [] = ""
parseDList i minus ((DList []):xs) = replicate ((i - minus) * 2) ' ' ++ "- []\n" ++ parseDList i 0 xs
parseDList i minus ((DList x):xs) = replicate ((i-minus) * 2) ' ' ++ "- " ++ parseDList (i + 1) (i + 1) x ++ parseDList i 0 xs -- buvo replicate (i * 2) ' ' ++ "-" ++ parseDList (i + 1) i x ++ parseDList i 0 xs
parseDList i minus ((DMap []):xs) = replicate ((i-minus) * 2) ' ' ++ "- " ++ "{}\n" ++ parseDList i 0 xs --buvo"- "
parseDList i minus ((DMap x):xs) = replicate ((i - minus) * 2) ' ' ++ "- " ++ parseDMap (i + 1) 0 x ++ parseDList i 0 xs
parseDList i minus ((DInteger x):xs) = replicate ((i - minus) * 2) ' ' ++ "- " ++ show x ++ "\n" ++ parseDList i 0 xs
parseDList i minus ((DString x):xs)  = replicate ((i - minus) * 2) ' ' ++ "- " ++ parseDString x ++ "\n" ++ parseDList i 0 xs --check DList nesamone cia -- pats tu nesąmonė
parseDList i minus ((DNull:xs)) = replicate ((i - minus) * 2) ' ' ++ "- " ++ "null" ++ "\n" ++ parseDList i 0 xs


parseDMap :: Int -> Int ->[(String, Document)] -> String
parseDMap _ _ [] = ""
parseDMap i mult ((x, DMap []):xs) = replicate (i * 2 * mult) ' ' ++ parseDString x ++ ": " ++ "{}\n" ++ parseDMap i 1 xs
parseDMap i mult ((x, DMap y):xs) = replicate (i * 2 * mult) ' ' ++ parseDString x ++ ":\n" ++ parseDMap (i + 1) 1 y ++ parseDMap i 1 xs
parseDMap i mult ((x, DList []):xs) =  replicate (i * 2 * mult) ' ' ++ parseDString x ++ ": " ++ "[]\n"  ++ parseDMap i 1 xs
parseDMap i mult ((x, DList y):xs) =  replicate (i * 2 * mult) ' ' ++ parseDString x ++ ":\n" ++ parseDList i 0 y  ++ parseDMap i 1 xs --(parseDList (i+1) buvo)
parseDMap i mult ((x, DInteger y):xs) = replicate (i * 2 * mult) ' ' ++ parseDString x ++ ": " ++ show y ++ "\n" ++ parseDMap i 1 xs
parseDMap i mult ((x, DString y):xs) = replicate (i * 2 * mult) ' ' ++ parseDString x ++ ": " ++ parseDString y ++ "\n" ++ parseDMap i 1 xs
parseDMap i mult ((x, DNull):xs) = replicate (i * 2 * mult) ' ' ++ parseDString x ++ ": null" ++ "\n" ++ parseDMap i 1 xs

parseDString :: String -> String
parseDString str
    | str == "" = "''"
    | str == "Yes" || str == "yes" ||
      str == "No"  || str == "no"  ||
      str == "n" = "'" ++ str ++ "'"
    | isStringNumber str = "'" ++ str ++ "'"
    | take 1 (reverse str) == " " = "'" ++ str ++ "'"
    | take 1 str == " " = "'" ++ str ++ "'"
    | otherwise = str

isStringNumber :: String -> Bool
isStringNumber str = 
    let 
        number = readMaybe str :: Maybe Int
    in
        case number of
            Just _ -> True
            Nothing -> False
-------------------------------------------------------------------

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error
gameStart :: State -> Document -> Either String State
gameStart (State a b c d) e =
    if a == 0 && null b && null c && null d && isDocumentCorrect e
    then moveWhenStateCorrect (State a b c d) e
    else Left wrongState

isDocumentCorrect :: Document -> Bool
isDocumentCorrect (DList _)  = False
isDocumentCorrect DNull      = False
isDocumentCorrect (DInteger _) = False
isDocumentCorrect (DString _) = False
isDocumentCorrect (DMap []) = False
isDocumentCorrect _ = True


moveWhenStateCorrect :: State -> Document -> Either String State
moveWhenStateCorrect (State x y z u) a = do
    let keys = keysFound ["number_of_hints", "occupied_rows", "occupied_cols"] a
    case keys of
        Left str -> Left (str ++ notFoundKey)
        Right _ -> continueGameStart (State x y z u) a


keysFound :: [String] -> Document -> Either String Document
keysFound [] d = Right d
keysFound (key:keys) a =
    if findSubstring key (toMap a) == Left (key ++ notFoundKey)
    then Left key
    else keysFound keys a


continueGameStart :: State -> Document -> Either String State
continueGameStart (State _ _ _ d) e = do
        x <- checkNumHints(findSubstring "number_of_hints" (toMap e))
        y <- makeList(findSubstring "occupied_rows" (toMap e))
        z <- makeList(findSubstring "occupied_cols" (toMap e))
        return (State x y z d)


makeList :: Either String Document -> Either String [Int]
makeList (Right (DMap xs)) = do
    let correctness = checkDMap xs xs "head"
    case correctness of
        Left e -> Left e
        Right _ -> parseFurther xs
makeList _ = Left "improper type when creating a list"


parseFurther :: [(String, Document)] -> Either String [Int]
parseFurther (x:xs) = do
    let first = getTail xs
    case first of
        Right a -> Right $ toNum' (snd x) : a
        Left b -> Left b
parseFurther _ = Left "Unexpected error in parseFurther"


checkDMap :: [(String, Document)] -> [(String, Document)] -> String -> Either String [(String, Document)]
checkDMap ((x1, DInteger x2):xs) a str =
    if x1 == str && x2 >= 0 && x2 <= 9
    then checkDMap xs a "tail"
    else Left "Found wrong value when checking DMap"
checkDMap ((x1, DMap x2):_) a str =
    if x1 == str
    then checkDMap x2 a "head"
    else Left "Found wrong value when checking DMap"
checkDMap [(x1, DNull)] a _ =
    if x1 == "tail"
    then Right a
    else Left "Found wrong value when checking DMap"
checkDMap ((_, _):_) _ _ = Left "Found wrong value when checking DMap"
checkDMap _ _ _ = Left "Unexpected error in checkDMap"


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


findSubstring :: String -> [(String, Document)] -> Either String Document
findSubstring key [] = Left (key ++ " key not found or is placed incorrectly")
findSubstring key a =
    if key == fst (head a)
    then Right $ snd (head a)
    else findSubstring key (tail a)

------------------------------------------------------------------

wrongHintState :: String
wrongHintState = "Wrong State Hint data"

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error
hint :: State -> Document -> Either String State
hint (State a b c d) t =
    if isDocumentCorrect t
    then checkFurther (State a b c d) t
    else Left wrongHintState

checkFurther :: State -> Document -> Either String State
checkFurther (State a b c d) t = do
    let x = if isDocumentCorrect t then checkHintLength $ toMap t else Left "improper document type"
    case x of
        Right _ -> makeHint (State a b c d) t
        Left y -> Left y


makeHint :: State -> Document -> Either String State
makeHint (State a b c d) t = do
    let x = fillUpHints (toMap t)
    case x of
        Right q -> return (State a b c (q ++ d))
        Left y -> Left y

checkHintLength :: [(String, Document)] -> Either String Bool
checkHintLength [a] = checkHintStructure a
checkHintLength _ = Left "There has to be exactly one element in Hint DMap"

checkHintStructure :: (String, Document) -> Either String Bool
checkHintStructure ("coords", DList a) = checkFurtherHintStructure a
checkHintStructure (_,_) = Left "Tuple must be made out of \"coords\" key and a DList value"

checkFurtherHintStructure :: [Document] -> Either String Bool
checkFurtherHintStructure [] = Left "Hint coordinate list must not be empty"
checkFurtherHintStructure [(DMap [(x1, DInteger y1), (x2, DInteger y2)])] =
    if x1 == "col" && x2 == "row" && y1 >= 0 && y1 <= 9 && y2 >= 0 && y2 <= 9
    then Right True
    else Left "Bad value in one on Hint's DMap values"
checkFurtherHintStructure ((DMap [(x1, DInteger y1), (x2, DInteger y2)]):xs) =
    if x1 == "col" && x2 == "row" && y1 >= 0 && y1 <= 9 && y2 >= 0 && y2 <= 9
    then checkFurtherHintStructure xs
    else Left "Bad value in one on Hint's DMap values"
checkFurtherHintStructure ((DMap [(_, _), (_, _)]):_) = Left "Tuple most contain a string key and a DInteger value"
checkFurtherHintStructure [(DMap [])] = Left "Not enough values in one of Hint's DMaps"
checkFurtherHintStructure [(DMap [_])] = Left "Not enough values in one of Hint's DMaps"
checkFurtherHintStructure ((DMap []):_) = Left "Not enough values in one of Hint's DMaps"
checkFurtherHintStructure ((DMap [_]):_) = Left "Not enough values in one of Hint's DMaps"
checkFurtherHintStructure ((DMap (_:_:_)):_) = Left "Too many values in one of Hint's DMaps"
checkFurtherHintStructure (_:_) = Left "DList must contain only DMaps with 2 tuples each"


fillUpHints :: [(String, Document)] -> Either String [Coord]
fillUpHints (x:_) = do
    let first = getElements (toList' $ snd x)
    case first of
        Right a -> return a
        Left b -> Left b
fillUpHints _ = Left "Error in fillUpHints"


getElements :: [Document] -> Either String [Coord]
getElements [] = Right []
getElements ((DMap [x,y]):xs) = do
    let first = getElements xs
    case first of
        Right a -> return $ Coord (toNum' $ snd x) (toNum' $ snd y) : a
        Left b -> Left b
getElements _ = Left "Error in getElements"

------------------------------------------------------------------