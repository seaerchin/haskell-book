{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chap31 where

import Control.Exception
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Tuple.Extra
import Data.Typeable
import Database.SQLite.Simple hiding (bind, close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

sockAddr = "8080"

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  (soc, _) <- accept sock
  printAndKickback soc
  close soc
  where
    printAndKickback conn = do
      msg <- recv conn 1024
      print msg
      sendAll conn msg

main :: IO ()
main = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just sockAddr)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  logAndEcho sock
  close sock

data User = User
  { userId :: Integer,
    username :: Text,
    shell :: Text,
    homeDirectory :: Text,
    realName :: Text,
    phone :: Text
  }
  deriving (Eq, Show)

type SocketInfo = (Connection, Socket)

-- realsocket describes a (connection, socket) pair rooted in the IO monad
-- that is, the type is able to interact with the real world meaningfully
type RealSocket a = ReaderT SocketInfo IO a

getConnection :: RealSocket Connection
getConnection = do
  fst <$> ask

getSocket :: RealSocket Socket
getSocket = do
  snd <$> ask

instance FromRow User where fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where toRow (User id_ username shell homeDir realName phone) = toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers =
  [r| CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT UNIQUE, shell TEXT, homeDirectory TEXT, realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Text -> RealSocket (Maybe User)
getUser username = do
  conn <- getConnection
  results <- liftIO $ query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> liftIO $ throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where
    meRow :: UserRow
    meRow =
      ( Null,
        "callen",
        "/bin/zsh",
        "/home/callen",
        "Chris Allen",
        "555-123-4567"
      )

returnUsers :: RealSocket ()
returnUsers = do
  soc <- getSocket
  dbConn <- getConnection
  rows <- liftIO $ query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  liftIO $ sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat ["Login: ", e username, "\t\t\t\t", "Name: ", e realName, "\n", "Directory: ", e homeDir, "\t\t\t", "Shell: ", e shell, "\n"]
  where
    e = encodeUtf8

returnUser :: Text -> RealSocket ()
returnUser username = do
  soc <- getSocket
  dbConn <- getConnection
  maybeUser <- getUser (T.strip username)
  case maybeUser of
    Nothing -> do
      liftIO $ putStrLn ("Couldn't find matching user for username: " ++ show username)
      return ()
    Just user -> liftIO $ sendAll soc (formatUser user)

handleQuery :: RealSocket ()
handleQuery = do
  soc <- getSocket
  msg <- liftIO $ recv soc 1024
  case msg of
    "\r\n" -> returnUsers
    name -> returnUser (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries conn sock = forever $ do
  (soc, _) <- accept sock

  putStrLn "Got connection, handling query"
  runReaderT handleQuery (conn, soc)
  close soc

m :: IO ()
m = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just sockAddr)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1 -- only one connection open at a time
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  close sock
