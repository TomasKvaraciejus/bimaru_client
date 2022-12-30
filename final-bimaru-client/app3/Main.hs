{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy as L (evalState)
-- import Data.ByteString.Lazy.Internal as I
import Control.Monad.Trans.State.Strict
    ( evalStateT, get, modify, put, StateT)
import Data.ByteString as B (ByteString)
-- import Data.Either as E (fromRight)
import Data.ByteString.Internal as C
import qualified Data.List as L
import Text.Read (readMaybe)
import Data.Text as T ( concat, pack, unpack, Text )
-- import Data.Text.Encoding.Base64 (decodeBase64)
import Data.Text.IO as TIO ( hPutStrLn, putStrLn )
import Data.List.Split as S ( splitOn )
import Data.Char (isSpace)
import Lib1 ( emptyState, mkCheck, render, toggle, State, State(..) )
import Lib2 ( renderDocument, toDocument )
import Lib3 ( parseDocument, gameStart, GameStart, GameStart(..))
import Types(Check(..), Check, Coord, fromDocument, fDocument)
import Network.Wreq
    (postWith, defaults, header, responseBody )
import qualified Network.Wreq as Wreq

import Control.Lens
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import Data.String.Conversions

type Repl a = HaskelineT (StateT ((String, Lib1.State), Int) IO) a

commandShow :: String
commandShow = "show"

commandCheck :: String
commandCheck = "check"

commandToggle :: String
commandToggle = "toggle"

commandSave :: String
commandSave = "save"

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd c
  | trim c == commandShow = lift get >>= liftIO . Prelude.putStrLn . Lib1.render . snd . fst
  | trim c == commandCheck = lift get >>= check . (Lib1.mkCheck . snd . fst) >>= liftIO . Prelude.putStrLn
  | trim c == commandSave = lift get >>= save
  | commandToggle `L.isPrefixOf` trim c = do
    case tokens c of
      [_] -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandToggle ++ "\" expects at least one argument"
      t -> if even $ length t
           then liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandToggle ++ "\" expects an EVEN number of arguments"
           else lift $ modify (\((u, s), i) -> ((u, Lib1.toggle s (L.drop 1 t)), i))
cmd c = liftIO $ Prelude.putStrLn $ "Unknown command: " ++ c

tokens :: String -> [String]
tokens s = L.filter (not . Prelude.null) $ S.splitOn " " s

trim :: String -> String
trim = f . f
  where f = L.reverse . L.dropWhile isSpace

check :: Check -> Repl String
check c = do
  ((url, _), _) <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = cs $ renderDocument $ toDocument c :: B.ByteString
  let trimmedBody = C.unpackChars body
  resp <- liftIO $ postWith opts (url ++ "/check") (C.packChars trimmedBody)
  pure $ cs $ resp ^. responseBody

save :: ((String, Lib1.State), Int) -> Repl ()
save ((url, State _ _ t), i) = do
  let coordDList = Lib2.toDocument $ Check t
  r <- liftIO $ Wreq.post (url ++ "/save/" ++ show i) (packChars $ Lib2.renderDocument coordDList)
  liftIO $ TIO.putStrLn $ T.pack $ show $ r ^. responseBody

completer :: Monad m => WordCompleter m
completer n = do
  let names = [commandShow, commandCheck, commandToggle, commandSave]
  return $ Prelude.filter (L.isPrefixOf n) names

ini :: Repl ()
ini = do
  ((url, s), i) <- lift get
  let url' = if i == 0 then url ++ "/add" else url ++ "/load"
  r <- liftIO $ Wreq.get url'
  let h = L.evalState $ runExceptT Lib3.parseDocument
  let h' = h (cs (r ^. responseBody))
  let gs = (except h' >>= fromDocument) :: ExceptT String IO Lib3.GameStart
  gs' <- liftIO $ runExceptT gs
  case gs' of
    Left msg -> liftIO $ fatal $ cs msg
    (Right (GameStart a b c)) -> do
      let gId = if url' == url ++ "/add" then a else i
      lift $ put ((url, Lib3.gameStart s (GameStart gId b c)), gId)
      liftIO $ TIO.putStrLn $ T.pack $ "Your game id: #" ++ show gId
      liftIO $ TIO.putStrLn "Welcome to Bimaru v3. Press [TAB] for available commands list"


fatal :: Text -> IO ()
fatal msg = do
  TIO.hPutStrLn stderr $ T.concat ["ERROR: ", msg]
  exitFailure

final :: Repl ExitDecision
final = do
  liftIO $ TIO.putStrLn "Goodbye!"
  return Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> run "0" 
    [token] -> run token
    _ -> fatal "token not provided, expected at least one command argument"

run :: String -> IO ()
run token = do
  let num = getNum $ readMaybe token
  let fullUrl = T.unpack "http://localhost:6969"
  let loadUrl = if num == 0 then T.unpack "http://localhost:6969" else T.unpack "http://localhost:6969/load/" ++ show num
  startingState <- getStartingState num loadUrl
  evalStateT (evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final) ((fullUrl, startingState), num)

getStartingState :: Int -> String -> IO Lib1.State
getStartingState 0 _ = return Lib1.emptyState
getStartingState _ url = do
  r <- liftIO $ Wreq.get url
  let h = L.evalState $ runExceptT Lib3.parseDocument
  let h' = h (cs (r ^. responseBody))
  let gs = (except h' >>= fDocument) :: ExceptT String IO [Coord]
  gs' <- liftIO $ runExceptT gs
  case gs' of
    Left _ -> return Lib1.emptyState
    Right d -> do
      case Lib1.emptyState of
        (State a b _) -> return $ State a b d

getNum :: Maybe Int -> Int
getNum Nothing = 0
getNum (Just a) = a
