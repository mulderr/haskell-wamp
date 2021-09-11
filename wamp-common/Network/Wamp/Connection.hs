{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

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
  , WampException (..)

  , receiveMessage
  , sendMessage

  , acceptWsRequest
  , parseWsMessage
  , writeWsMessage
  , closeWsConnection
  )
where

import           Control.Exception     (Exception (..))
import           Data.Aeson
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import           Data.List             (intersect)
import           Data.Maybe            (listToMaybe)
import           Data.Typeable         (Typeable)
import qualified Network.WebSockets    as WS

import           Network.Wamp.Messages
import           Network.Wamp.Types


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
  { connectionParse :: !(IO (Maybe Message))
  , connectionWrite :: !(Message -> IO ())
  , connectionClose :: !(IO ())
  }


-- | Router session
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

  ws <- WS.acceptRequestWith pc (WS.AcceptRequest msp [])
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


data WampException
  -- | Peer broke protocol. We can no longer make any assumptions about
  -- this session. There is not a single valid message we can reply with,
  -- so the session must be silently closed.
  = ProtocolException String
  -- | Received ERROR as reply to our request
  -- | (one of subscribe,unsubscribe,publish,register,unregister).
  | RequestFailed Details ErrorUri
  -- | An RPC has failed (error returned by dealer or callee)
  | RpcError Details ErrorUri Arguments ArgumentsKw
  -- | Received @Goodbye@, replied @Goodbye@, session is over, time for cleanup.
  | SessionClosed
  deriving (Show, Typeable)

instance Exception WampException
