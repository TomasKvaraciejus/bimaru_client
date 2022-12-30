{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Main (main) where

import qualified Network.HTTP.Types as HTTP
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
import Data.List (sort)
import Data.Text.Lazy as D
import Data.ByteString.Internal as I
import Data.Yaml as Y
import GHC.Generics
import Web.Scotty as W
import Control.Monad.State.Lazy as S


data Game = Game {
  gameId :: Int,
  coords :: M.Map Int [Coord]
} deriving (Generic, Show)

data Coord = Coord {
    col :: Int,
    row :: Int
} deriving (Generic, Show, Eq, Ord)



iniState :: Int -> String
iniState i = "gameId: " ++ show i ++ "\noccupied_rows:\n  head: 3\n  tail:\n    head: 3\n    tail:\n      head: 0\n      tail:\n        head: 0\n        tail:\n          head: 3\n          tail:\n            head: 0\n            tail:\n              head: 5\n              tail:\n                head: 0\n                tail:\n                  head: 4\n                  tail:\n                    head: 2\n                    tail: null\noccupied_cols:\n  head: 1\n  tail:\n    head: 1\n    tail:\n      head: 4\n      tail:\n        head: 2\n        tail:\n          head: 2\n          tail:\n            head: 2\n            tail:\n              head: 2\n              tail:\n                head: 2\n                tail:\n                  head: 0\n                  tail:\n                    head: 4\n                    tail: null\n"

winState :: [Coord]
winState = sort [Coord 0 2, Coord 1 2, Coord 0 6, Coord 1 6, Coord 0 9, Coord 1 9, Coord 4 0, Coord 4 1, Coord 4 2, Coord 6 3, Coord 6 4, Coord 6 5, Coord 6 7, Coord 6 9, Coord 8 2, Coord 8 3, Coord 8 4, Coord 8 5, Coord 9 7, Coord 9 9]


main :: IO ()
main = do
  putStrLn "Starting server..."
  s <- STM.newTVarIO Game{gameId = 1, coords = M.empty}
  W.scotty 6969 $ runRoutes s
    

runRoutes :: STM.TVar Game -> W.ScottyM ()
runRoutes st = do
    W.get "/load/:id" $ do
      gid <- param "id"
      games <- liftIO $ coords <$> STM.readTVarIO st
      case M.lookup gid games of
        (Just a) -> text (D.pack $ unpackChars $ Y.encode a)
        Nothing -> do
          W.status HTTP.notFound404
          text "404 Game not found."
      
    W.get "/load" $ do
      text (D.pack $ iniState 0)

    W.get "/add" $ do
      gid <- liftIO $ addNewGame st
      text (D.pack $ iniState gid)

    W.post "/save/:id" $ do
      gid <- param "id"
      r <- bodyReader >>= liftIO
      case Y.decodeEither' r of
        Right r' -> do
          exists <- liftIO $ STM.atomically $ do
            current <- STM.readTVar st
            case M.lookup gid (coords current) of
              Just _ -> do
                let f _ = Just r'
                STM.writeTVar
                  st
                  ( current
                    { coords = M.update f gid (coords current)}
                  )
                pure True
              
              Nothing -> pure False
          if exists
          then text $ mconcat["Game of id #", D.pack $ show gid ," saved successfully!"]
          else W.status HTTP.notFound404 >> W.text "404 Game not found"
        Left _ -> W.status HTTP.notFound404 >> W.text "404 Game not found"

    W.post "/check" $ do
      r <- bodyReader >>= liftIO
      case Y.decodeEither' r of
        Right a -> if winState == sort a then text "You win!" else text "Try again!"
        Left _  -> W.status HTTP.notFound404 >> text "Could not check the coordinates."

addNewGame :: STM.TVar Game -> IO Int
addNewGame s = do
  STM.atomically $ do
    thestate <- STM.readTVar s
    STM.writeTVar
      s
      ( thestate
        { gameId = gameId thestate + 1
        , coords = M.insert (gameId thestate) [] (coords thestate)
        }
      )
    pure (gameId thestate)


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

