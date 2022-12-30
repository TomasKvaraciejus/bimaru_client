module Helper(parseDocument, renderDocument, fDocument, fromDocument, toDocument) where

import Util
import Types
import Lib
import Text.Read (readMaybe)
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy as S (State, get)

-----------------------------------------------------------------
-----------------------PARSE DOCUMENT----------------------------
-----------------------------------------------------------------

instance FDocument Document where
    fDocument (DList a) = return $ makeCoordList a
        where
            makeCoordList :: [Document] -> [Coord]
            makeCoordList [] = []
            makeCoordList ((DMap [(_,DInteger u), (_, DInteger v)]):xs) = Coord u v : makeCoordList xs
            makeCoordList _ = []
    fDocument _ = throwE "Not propper coord list"

instance FromDocument GameStart where 
  fromDocument e = do
        x <- getNum $ findSubstring "gameId" (toMap e)
        y <- except $ makeList(findSubstring "occupied_rows" (toMap e))
        z <- except $ makeList(findSubstring "occupied_cols" (toMap e))
        return $ GameStart x y z


type CustomExceptT a = ExceptT String (S.State String) a

parseDocument :: CustomExceptT Document
parseDocument = S.get >>= \str ->
    if take 1 (reverse str) == "\n" 
    then if checkDocument str (spacingLength str) 
        then createDocument str
        else throwE "incorrect document format"
    else throwE "No '\\n' at file end"

createType :: String -> CustomExceptT Document -- creates base type (DString, DInteger, DNull)
createType str
    | elem '\'' (readLine str) || elem '\"' (readLine str) = return $ DString $ getQuotedValue str 
    | removeForwardSpacing (removeBackSpacing $ readLine str) == "null" || removeForwardSpacing (removeBackSpacing $ readLine str) == "~" = return DNull
    | isInteger (removeForwardSpacing (removeBackSpacing (readLine str))) False = parseDInteger $ readLine str
    | otherwise = return $ DString $ removeForwardSpacing $ removeBackSpacingDMap (readLine str)

createDocument :: String -> CustomExceptT Document
createDocument str
    | removeBackSpacingDMap (readLine str) == "[]" = return $ DList []
    | removeBackSpacingDMap (readLine str) == "{}" = return $ DMap []
    | elem '-' (readLine str) && (take 1 (dropUntil str '-') == " ")
        = do
        DList <$> ((createDList str (spacingLength str)))
    | elem ':' (readLine str)
        = do
            a <- createDMap str (spacingLength str)
            return $ DMap a
    
    | otherwise =  if checkType str 
                   then createType $ str
                   else throwE "Bad type"

createDList :: String -> Int -> CustomExceptT [Document]
createDList [] _ = return []
createDList str n =
    do
        a <- createDListElement $ replaceDash str
        b <- if str == [] then return [] else
            do
                c <- dropUntilSameSpacing (dropUntil str '\n') n False
                createDList c n
        return $ a : b

createDListElement :: String -> CustomExceptT Document
createDListElement = createDocument

createDMap :: String -> Int -> CustomExceptT [(String, Document)] -- recursively creates DMap
createDMap [] _ = return []
createDMap str n =
    do
        a <- parseDMap str -- a = (key, value)
        b <- if str == [] then return [] else 
            do
            s <- dropUntilSameSpacing (dropUntil str '\n') n True -- if isValueOnSameLine str
            createDMap s n -- b = recursively created DMap to append take 1 (drop (getLengthUntil str ':' + 1) str)
        return $ a : b

parseDMap :: String -> CustomExceptT (String, Document)
parseDMap str = do
    key    <- checkDMapKey $ readLine str
    value  <- parseDMapValue str
    return (key, value)

parseDInteger :: String -> CustomExceptT Document
parseDInteger str
    | elem '-' str =
        if not (isInteger (dropUntil str '-') False) then throwE $ "incorrect integer format. " ++ errorMsg str
        else return $ DInteger $ read (drop (getLengthUntil str '-') str)
    | otherwise = return $ DInteger $ read str

parseDMapValue :: String -> CustomExceptT Document
parseDMapValue str 
    | isValueOnSameLine str = createDocument $ (dropUntil (readLine str) ':')
    | otherwise = if spacingLength (getNextLine str) >= spacingLength str
                  then 
                    if (spacingLength str > (spacingLength (getNextLine str))) && (elem ':' (getNextLine str)) 
                    then return DNull
                    else createDocument $ dropUntil str '\n'
                  else 
                    throwE $ "DMap value on same height as key. " ++ errorMsg str


-----------------------------------------------------------------
-----------------------RENDER DOCUMENT---------------------------
-----------------------------------------------------------------

instance ToDocument Check where
    toDocument a = DList $ makeDList a
        where
            makeDList :: Check -> [Document]
            makeDList (Check []) = []
            makeDList (Check ((Coord x y):xs)) = DMap [("col", DInteger x), ("row", DInteger y)] : makeDList (Check xs)


renderDocument :: Document -> String
renderDocument (DMap []) = "{}\n"
renderDocument (DList []) = "[]\n"
renderDocument (DMap a) = renderDMap 0 1 a
renderDocument (DList a) =  renderDList 0 1 a
renderDocument (DString a)  = renderDString a  ++ "\n"
renderDocument DNull = "null\n"
renderDocument (DInteger a) = show a ++ "\n"

renderDList :: Int -> Int -> [Document] -> String
renderDList _ _ [] = ""
renderDList i minus ((DList []):xs) = replicate ((i - minus) * 2) ' ' ++ "- []\n" ++ renderDList i 0 xs
renderDList i minus ((DList x):xs) = replicate ((i-minus) * 2) ' ' ++ "- " ++ renderDList (i + 1) (i + 1) x ++ renderDList i 0 xs -- buvo replicate (i * 2) ' ' ++ "-" ++ renderDList (i + 1) i x ++ renderDList i 0 xs
renderDList i minus ((DMap []):xs) = replicate ((i-minus) * 2) ' ' ++ "- " ++ "{}\n" ++ renderDList i 0 xs --buvo"- "
renderDList i minus ((DMap x):xs) = replicate ((i - minus) * 2) ' ' ++ "- " ++ renderDMap (i + 1) 0 x ++ renderDList i 0 xs
renderDList i minus ((DInteger x):xs) = replicate ((i - minus) * 2) ' ' ++ "- " ++ show x ++ "\n" ++ renderDList i 0 xs
renderDList i minus ((DString x):xs)  = replicate ((i - minus) * 2) ' ' ++ "- " ++ renderDString x ++ "\n" ++ renderDList i 0 xs --check DList nesamone cia -- pats tu nesąmonė
renderDList i minus ((DNull:xs)) = replicate ((i - minus) * 2) ' ' ++ "- " ++ "null" ++ "\n" ++ renderDList i 0 xs

renderDMap :: Int -> Int ->[(String, Document)] -> String
renderDMap _ _ [] = ""
renderDMap i mult ((x, DMap []):xs) = replicate (i * 2 * mult) ' ' ++ renderDString x ++ ": " ++ "{}\n" ++ renderDMap i 1 xs
renderDMap i mult ((x, DMap y):xs) = replicate (i * 2 * mult) ' ' ++ renderDString x ++ ":\n" ++ renderDMap (i + 1) 1 y ++ renderDMap i 1 xs
renderDMap i mult ((x, DList []):xs) =  replicate (i * 2 * mult) ' ' ++ renderDString x ++ ": " ++ "[]\n"  ++ renderDMap i 1 xs
renderDMap i mult ((x, DList y):xs) =  replicate (i * 2 * mult) ' ' ++ renderDString x ++ ":\n" ++ renderDList i 0 y  ++ renderDMap i 1 xs --(renderDList (i+1) buvo)
renderDMap i mult ((x, DInteger y):xs) = replicate (i * 2 * mult) ' ' ++ renderDString x ++ ": " ++ show y ++ "\n" ++ renderDMap i 1 xs
renderDMap i mult ((x, DString y):xs) = replicate (i * 2 * mult) ' ' ++ renderDString x ++ ": " ++ renderDString y ++ "\n" ++ renderDMap i 1 xs
renderDMap i mult ((x, DNull):xs) = replicate (i * 2 * mult) ' ' ++ renderDString x ++ ": null" ++ "\n" ++ renderDMap i 1 xs


renderDString :: String -> String
renderDString str
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