{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Wamp.Connection
-- Description : Connection
-- Copyright   : (c) Maciej Kazulak, 2015
-- License     : MIT
-- Maintainer  : kazulakm@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- WAMP Connection.
--
module Network.Wamp.Connection
  ( Connection (..)
  , Session (..)

  , receiveMessage
  , sendMessage

  , acceptWsRequest
  , parseWsMessage
  , writeWsMessage
  , closeWsConnection
  )
where

import           Data.Aeson
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import           Data.List             (intersect)
import           Data.Maybe            (listToMaybe)
import qualified Network.WebSockets    as WS

import Network.Wamp.Types
import Network.Wamp.Messages


-- supported websocket subprotocols in order of descending preference
-- no support for msgpack yet, sorry
supportedSubprotocols :: [B.ByteString]
supportedSubprotocols =
  [ "wamp.2.json"
  ]

defaultPingDelay :: Int
defaultPingDelay = 30

-- | WAMP connection
--
-- Abstracts away the underlying transport.
data Connection = Connection
  { connectionParse     :: !(IO (Maybe Message))
  , connectionWrite     :: !(Message -> IO ())
  , connectionClose     :: !(IO ())
  }


-- | WAMP session
data Session = Session
  { sessionId         :: !SessId
  , sessionConnection :: !Connection
  }

instance Show Session where
  show (Session sessId _) = show sessId

instance Eq Session where
  x == y = sessionId x == sessionId y

instance Ord Session where
  compare x y = sessionId x `compare` sessionId y


-- | Receive message
receiveMessage :: Connection -> IO Message
receiveMessage conn = do
  mMsg <- connectionParse conn
  case mMsg of
    Nothing  -> receiveMessage conn
    Just msg -> return msg


-- | Send message
sendMessage :: Connection -> Message -> IO ()
sendMessage conn msg = connectionWrite conn msg


-- | Accept a WebSocket connection
acceptWsRequest :: WS.PendingConnection -> IO Connection
acceptWsRequest pc = do
  let msp = chooseWsSubprotocol pc

  ws <- WS.acceptRequestWith pc (WS.AcceptRequest msp)
  WS.forkPingThread ws defaultPingDelay
  
  return $ Connection
      { connectionParse     = parseWsMessage ws
      , connectionWrite     = writeWsMessage ws
      , connectionClose     = closeWsConnection ws
      }

-- | Read a message from a WebSocket
parseWsMessage :: WS.Connection -> IO (Maybe Message)
parseWsMessage ws = do
  d <- WS.receiveData ws
  return $ decode d


-- | Send a message over a WebSocket
writeWsMessage :: WS.Connection -> Message -> IO ()
writeWsMessage ws msg = do
  WS.sendTextData ws $ encode msg


-- | Close WebSocket connection
closeWsConnection :: WS.Connection -> IO ()
closeWsConnection ws = WS.sendClose ws ("WAMP session closed" :: BL.ByteString)


chooseWsSubprotocol :: WS.PendingConnection -> Maybe B.ByteString
chooseWsSubprotocol pc = do
  let clientProtocols = WS.getRequestSubprotocols $ WS.pendingRequest pc
  listToMaybe $ supportedSubprotocols `intersect` clientProtocols