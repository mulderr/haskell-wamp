{-# LANGUAGE OverloadedStrings #-}

import           Prelude                        hiding (lookup)
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.Async       (async, link)
--import           Control.Exception              (throwIO, catch, finally)
import           Control.Monad                  (forever)

import           Network.Wamp.Types
import           Network.Wamp.Messages
import           Network.Wamp.Connection hiding (Session (..))
import           Network.Wamp.State
import           Network.Wamp.Client

main :: IO ()
main = do
  runClientWebSocket True "realm1" "api.poloniex.com" 443 "/" testApp


newsH :: Handler
newsH (Arguments args) _ _ = do
  putStrLn $ show args


messageHandler :: Session -> Message -> IO ()
messageHandler session msg = do
  case msg of
    Subscribed reqId subId -> do
      mr <- lookup (sessionSubscribeRequests session) reqId
      case mr of
        Nothing -> putStrLn $ "Unsolicited message: " ++ show msg
        Just (SubscribeRequest m _ topicUri handler) -> do
          let s = Subscription subId topicUri handler (Options dict)
          delete (sessionSubscribeRequests session) reqId
          insertSubscription (sessionSubscriptions session) s
          putMVar m (Right s)

    Unsubscribed reqId -> do
      mr <- lookup (sessionUnsubscribeRequests session) reqId
      case mr of
        Nothing -> putStrLn $ "Unsolicited message: " ++ show msg
        Just (UnsubscribeRequest m _ subId) -> do
          delete (sessionUnsubscribeRequests session) reqId
          deleteSubscription (sessionSubscriptions session) subId
          putMVar m (Right True)

    Event subId pubId details arguments argumentsKw -> do
      mr <- lookupSubscription (sessionSubscriptions session) subId
      case mr of
        Nothing -> putStrLn $ "Unsolicited message: " ++ show msg
        Just (Subscription subId topicUri handler opts) -> do
          handler arguments argumentsKw details

    _ -> error "Unexpected message"


receiveLoop :: Session -> IO ()
receiveLoop session = forever $ do
  msg <- receiveMessage (sessionConnection session)
  messageHandler session msg


testApp :: WampApp
testApp session = do
  (async $ receiveLoop session) >>= link
  putStrLn $ "\x2713 Session established: " ++ show (sessionId session)

  res <- subscribe session "ticker" (Options dict) newsH >>= readMVar
  case res of
    Left err -> putStrLn $ show err
    Right sub -> do
      putStrLn $ "\x2713 Subscribed: " ++ show sub

      threadDelay $ 5*1000*1000

      ures <- unsubscribe session sub >>= readMVar
      case ures of
        Left err -> putStrLn $ show err
        Right _ -> putStrLn $ "\x2713 Unsubscribed"
