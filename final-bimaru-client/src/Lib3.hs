{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(gameStart, parseDocument, GameStart(..)) where

import Types
import Util
import Lib1 (State(..))
import Lib2 (isDocumentCorrect, wrongState, keysFound, notFoundKey, findSubstring, toMap, makeList, findSubstring)
import Control.Monad.Trans.Except
-- import Control.Monad.IO.Class
import Control.Monad.State.Lazy as S (State, get)

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


instance FDocument Document where
    fDocument (DList a) = return $ makeCoordList a
        where
            makeCoordList :: [Document] -> [Coord]
            makeCoordList [] = []
            makeCoordList ((DMap [(_,DInteger u), (_, DInteger v)]):xs) = Coord v u : makeCoordList xs
            makeCoordList _ = []
    fDocument _ = throwE "Not propper coord list"

instance FromDocument GameStart where 
  fromDocument e =
    if isDocumentCorrect e
    then moveWhenStateCorrect e
    else throwE wrongState

data GameStart = GameStart{
  gameId :: Int,
  rows :: [Int],
  cols :: [Int]
} deriving Show

moveWhenStateCorrect :: Document -> ExceptT String IO GameStart
moveWhenStateCorrect a = do
    let keys = keysFound ["occupied_rows", "occupied_cols"] a
    case keys of
        Left str -> throwE $ str ++ notFoundKey
        Right _ -> continueGameStart a

continueGameStart :: Document -> ExceptT String IO GameStart
continueGameStart e = do
        x <- getNum $ findSubstring "gameId" (toMap e)
        y <- except $ makeList(findSubstring "occupied_rows" (toMap e))
        z <- except $ makeList(findSubstring "occupied_cols" (toMap e))
        return $ GameStart x y z

getNum :: Either String Document -> ExceptT String IO Int
getNum (Right (DInteger a)) = return a
getNum _ = throwE "not an int"

gameStart :: Lib1.State -> GameStart -> Lib1.State
gameStart (State _ _ d) (GameStart _ f g) = State f g d