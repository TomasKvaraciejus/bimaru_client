{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Main (main) where

import qualified Data.Text as T ( Text )
import Network.HTTP.Types
import Control.Concurrent (forkIO, newEmptyMVar)
import Control.Concurrent.MVar as MV
-- import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
-- import Network.Wreq (responseBody)
import Network.Wai (requestBody)
import Control.Lens
import Data.Monoid ((<>))
import Data.Text
import Data.List
import Data.Text.Lazy as D
-- import Data.ByteString.Lazy.Internal as I
import Data.ByteString.Internal as I
-- import Data.Aeson as A
import Data.Yaml as Y
import GHC.Generics
import Web.Scotty as W
import System.IO as IO
-- import Control.Monad.Trans.State
import Control.Monad.State.Lazy as S
import System.Directory (doesFileExist, removeFile)

data Game = Game {
  gameId :: Int,
  currentState :: [Coord]
} deriving (Generic, Show)

data Coord = Coord {
    col :: Int,
    row :: Int
} deriving (Generic, Show, Eq, Ord)

textFile :: String
textFile = "test.txt"

iniState :: Int -> String
iniState i = "gameId: " ++ show i ++ "\noccupied_rows:\n  head: 3\n  tail:\n    head: 3\n    tail:\n      head: 0\n      tail:\n        head: 0\n        tail:\n          head: 3\n          tail:\n            head: 0\n            tail:\n              head: 5\n              tail:\n                head: 0\n                tail:\n                  head: 4\n                  tail:\n                    head: 2\n                    tail: null\noccupied_cols:\n  head: 1\n  tail:\n    head: 1\n    tail:\n      head: 4\n      tail:\n        head: 2\n        tail:\n          head: 2\n          tail:\n            head: 2\n            tail:\n              head: 2\n              tail:\n                head: 2\n                tail:\n                  head: 0\n                  tail:\n                    head: 4\n                    tail: null\n"

winState :: [Coord]
winState = sort [Coord 0 2, Coord 1 2, Coord 0 6, Coord 1 6, Coord 0 9, Coord 1 9, Coord 4 0, Coord 4 1, Coord 4 2, Coord 6 3, Coord 6 4, Coord 6 5, Coord 6 7, Coord 6 9, Coord 8 2, Coord 8 3, Coord 8 4, Coord 8 5, Coord 9 7, Coord 9 9]
-- winState = "- row: 2\n  col: 0\n- row: 2\n  col: 1\n- row: 6\n  col: 0\n- row: 6\n  col: 1\n- row: 9\n  col: 0\n- row: 9\n  col: 1\n- row: 0\n  col: 4\n- row: 1\n  col: 4\n- row: 2\n  col: 4\n- row: 3\n  col: 6\n- row: 4\n  col: 6\n- row: 5\n  col: 6\n- row: 7\n  col: 6\n- row: 9\n  col: 6\n- row: 2\n  col: 8\n- row: 3\n  col: 8\n- row: 4\n  col: 8\n- row: 5\n  col: 8\n- row: 7\n  col: 9\n- row: 9\n  col: 9\n"



main :: IO ()
main = do
  putStrLn "Starting server..."
  scotty 4200 $ do
    W.get "/load/:id" getGameStateRoute
    W.get "/load" getStartingStateRoute
    W.get "/add" addGameRoute
    W.post "/save/:id" modifyGameStateRoute
    W.post "/check" checkStateRoute

-- main' :: IO ()
-- main' = do
--   dic <- STM.newTVarIO M.empty
--   let runActionToIO m = renReaderT (runWebM m) dic
--   scottyT 4200 runActionToIO app

-- app :: ScottyT D.Text WebM ()
-- app = do
--   W.get "/load" $ do
--     let task = return $ iniState 0
--     m <- newEmptyMVar
--     tid <- liftIO $ forkIO $ do
--       result <- task
--       putMVar result
--     r <- takeMVar m
--     text r

getStartingStateRoute :: ActionM ()
getStartingStateRoute = do
 text (D.pack $ iniState 0)

getGameStateRoute :: ActionM ()
getGameStateRoute = do
  gameId <- W.param "id"
  let gId = gameId :: Int
  gameStateString <- liftIO (getGameState gId)
  text (D.pack$ unpackChars $ Y.encode gameStateString)

getStateListRoute :: ActionM ()
getStateListRoute = do
  idList <- liftIO getGameIdList
  text (D.pack idList)

modifyGameStateRoute :: ActionM ()
modifyGameStateRoute = do
  gameId <- param "id"
  let gId = gameId :: Int
  r <- bodyReader
  r' <- liftIO r
  let toggled = Y.decode r' :: Maybe [Coord]
  let toggleList = getCoordList toggled
  liftIO $ modifyGameState gId toggleList


checkStateRoute :: ActionM ()
checkStateRoute = do
  r <- bodyReader
  r' <- liftIO r
  let toggled = Y.decode r' :: Maybe [Coord]
  let toggledList = sort $ getCoordList toggled
  case toggledList == winState of
    True -> text "You win!"
    _ -> text "Try again!"


getCoordList :: Maybe [Coord] -> [Coord]
getCoordList (Just a) = a
getCoordList Nothing = []



addGameRoute :: ActionM ()
addGameRoute = do
  s <- liftIO addGame
  text $ D.pack s

------------------------------------------------------------

createFile :: IO ()
createFile = do
  i <- doesFileExist textFile
  case i of
    False -> IO.writeFile textFile (unpackChars "")


deleteFile :: IO ()
deleteFile = do
  i <- doesFileExist textFile
  case i of
    False -> putStrLn $ "No '" ++ textFile ++ "' found"
    _ -> removeFile textFile

------------------------------------------------------------

addGame :: IO String
addGame = do
  openFileHandle <- openFile textFile ReadWriteMode
  s <- hGetContents openFileHandle
  let games = Y.decode $ packChars s :: Maybe [Game]
  case Prelude.length (getGameList games) < 100000 of
    True -> do
      let newGame = addGame' games
      let gameId = getGameId newGame
      let newGameList = getGameList games ++ [newGame]
      IO.writeFile textFile (I.unpackChars $ Y.encode newGameList)
      hClose openFileHandle
      return $ iniState gameId
    _ -> do
      hClose openFileHandle
      return "No more then 5 games can be active"

getGameId :: Game -> Int
getGameId (Game i _) = i

addGame' :: Maybe [Game] -> Game
addGame' Nothing = Game{gameId = 1, currentState = []}
addGame' (Just xs) = do
  let lastGame = Prelude.last xs
  case lastGame of
    (Game i _) -> Game{gameId = i + 1, currentState = []}

getGameList :: Maybe [Game] -> [Game]
getGameList x =
  case x of
    Nothing -> []
    Just a -> a

------------------------------------------------------------

getGameState :: Int -> IO [Coord]
getGameState i = do
  s <- readFile textFile 
  let games = Y.decode $ packChars s :: Maybe [Game]
  let gameList = getGameList games
  case findGameById i gameList of
    Nothing -> return []
    Just (Game _ d) -> return d

findGameById :: Int -> [Game] -> Maybe Game
findGameById key [] = Nothing
findGameById key (x:xs) = 
  case x of
    (Game i _) ->
      if key == i
      then Just x
      else findGameById key xs

------------------------------------------------------------

modifyGameState :: Int -> [Coord] -> IO ()
modifyGameState i coords = do
  openFileHandle <- openFile textFile ReadWriteMode
  s <- hGetContents openFileHandle
  putStrLn s
  let games = Y.decode $ packChars s :: Maybe [Game]
  let modifiedGameList = modifyGameState' i coords (getGameList games)
  IO.writeFile textFile (I.unpackChars $ Y.encode modifiedGameList)
  hClose openFileHandle

modifyGameState' :: Int -> [Coord] -> [Game] -> [Game]
modifyGameState' _ _ [] = []
modifyGameState' key coords (x:xs) =
  case x of
    (Game i _) ->
      if key == i
      then Game{gameId = i, currentState = coords} : xs
      else x : modifyGameState' key coords xs 

------------------------------------------------------------

getGameIdList :: IO String
getGameIdList = do
  s <- readFile textFile 
  let games = Y.decode $ packChars s :: Maybe [Game]
  return $ show $ getGameIdList' $ getGameList games

getGameIdList' :: [Game] -> [Int]
getGameIdList' [] = []
getGameIdList' ((Game i _):xs) = i : getGameIdList' xs

------------------------------------------------------------

instance ToJSON Game

instance FromJSON Game where
    parseJSON = withObject "Game" $ \v -> Game
        <$> v .: "gameId"
        <*> v .: "currentState"

instance ToJSON Coord

instance FromJSON Coord where
    parseJSON = withObject "Coord" $ \v -> Coord
        <$> v .: "col"
        <*> v .: "row"

-- instance FromJSON State where
--   parseJSON = withObject "State" $ \v -> State
--         <$> v .: "hint"
--         <*> v .: "rows"
--         <*> v .: "cols"




-- thing :: ByteString
-- thing = I.packChars "{\"gameId\":1,\"currentState\":\"Hello\"}"

-- thing' :: ByteString
-- thing' = I.packChars "[{\"gameId\":1,\"currentState\":\"Hello\"}, {\"gameId\":2,\"currentState\":\"Goodbye\"}]"