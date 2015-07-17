{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Wamp.Messages
-- Description : Message definitions
-- Copyright   : (c) Maciej Kazulak, 2015
-- License     : MIT
-- Maintainer  : kazulakm@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- WAMP message definitions.
--
module Network.Wamp.Messages
  ( Message (..)
  , defaultError
  )
where

import           Control.Applicative
import           Data.Aeson            hiding (Error)
import           Data.Aeson.Types      hiding (Error, Options)
import qualified Data.Vector           as V

import Network.Wamp.Types


-- | WAMP message
-- 
-- See the <https://github.com/tavendo/WAMP/blob/master/spec/basic.md#message-definitions Message Definitions>
-- section of the protocol specification.
data Message
  -- Session 
  = Hello         RealmUri Details
  | Welcome       SessId Details
  | Abort         Details ReasonUri
  | Goodbye       Details ReasonUri
  | Error         MessageType ReqId Details ErrorUri Arguments ArgumentsKw
  -- PubSub
  | Publish       ReqId Options TopicUri Arguments ArgumentsKw
  | Published     ReqId PubId
  | Subscribe     ReqId Options TopicUri
  | Subscribed    ReqId SubId
  | Unsubscribe   ReqId SubId
  | Unsubscribed  ReqId
  | Event         SubId PubId Details Arguments ArgumentsKw
  -- RPC
  | Call          ReqId Options ProcedureUri Arguments ArgumentsKw
  | Result        ReqId Details Arguments ArgumentsKw
  | Register      ReqId Options ProcedureUri
  | Registered    ReqId RegId
  | Unregister    ReqId RegId
  | Unregistered  ReqId
  | Invocation    ReqId RegId Details Arguments ArgumentsKw
  | Yield         ReqId Options Arguments ArgumentsKw
  deriving (Show)


-- Unfortunatelly the spec defines some fields as optional and even recommends
-- (as in SHOULD) that they NOT be sent at all when empty. 
instance FromJSON Message where
  parseJSON = withArray "WAMP message" $ \a -> do
    msgType <- parseJSON $ V.head a
    case msgType of
      MsgTypeHello        -> Hello        <$> (a!1) <*> (a!2)
      MsgTypeWelcome      -> Welcome      <$> (a!1) <*> (a!2)
      MsgTypeAbort        -> Abort        <$> (a!1) <*> (a!2)
      MsgTypeGoodbye      -> Goodbye      <$> (a!1) <*> (a!2)
      MsgTypeError        -> Error        <$> (a!1) <*> (a!2) <*> (a!3) <*> (a!4) <*> opt emptyArgs a 5  <*> opt emptyKwArgs a 6

      MsgTypePublish      -> Publish      <$> (a!1) <*> (a!2) <*> (a!3) <*> opt emptyArgs a 4 <*> opt emptyKwArgs a 5
      MsgTypePublished    -> Published    <$> (a!1) <*> (a!2)
      MsgTypeSubscribe    -> Subscribe    <$> (a!1) <*> (a!2) <*> (a!3)
      MsgTypeSubscribed   -> Subscribed   <$> (a!1) <*> (a!2)
      MsgTypeUnsubscribe  -> Unsubscribe  <$> (a!1) <*> (a!2)
      MsgTypeUnsubscribed -> Unsubscribed <$> (a!1)
      MsgTypeEvent        -> Event        <$> (a!1) <*> (a!2) <*> (a!3) <*> opt emptyArgs a 4 <*> opt emptyKwArgs a 5

      MsgTypeCall         -> Call         <$> (a!1) <*> (a!2) <*> (a!3) <*> opt emptyArgs a 4 <*> opt emptyKwArgs a 5
      MsgTypeResult       -> Result       <$> (a!1) <*> (a!2) <*> opt emptyArgs a 3 <*> opt emptyKwArgs a 4
      MsgTypeRegister     -> Register     <$> (a!1) <*> (a!2) <*> (a!3)
      MsgTypeRegistered   -> Registered   <$> (a!1) <*> (a!2)
      MsgTypeUnregister   -> Unregister   <$> (a!1) <*> (a!2)
      MsgTypeUnregistered -> Unregistered <$> (a!1)
      MsgTypeInvocation   -> Invocation   <$> (a!1) <*> (a!2) <*> (a!3) <*> opt emptyArgs a 4 <*> opt emptyKwArgs a 5
      MsgTypeYield        -> Yield        <$> (a!1) <*> (a!2) <*> opt emptyArgs a 3 <*> opt emptyKwArgs a 4

      _                   -> fail $ "unimplemented message type: " ++ show msgType


instance ToJSON Message where
  toJSON m =
    case m of
      Hello         a b           -> toJsonMsg2         MsgTypeHello        a b
      Welcome       a b           -> toJsonMsg2         MsgTypeWelcome      a b
      Abort         a b           -> toJsonMsg2         MsgTypeAbort        a b
      Goodbye       a b           -> toJsonMsg2         MsgTypeGoodbye      a b
      Error         a b c d e f   -> Array $ array <&>  MsgTypeError        <&> a <&> b <&> c <&> d <&> e <&> f                             

      Publish       a b c d e     -> Array $ array <&>  MsgTypePublish      <&> a <&> b <&> c <&> d <&> e
      Published     a b           -> toJsonMsg2         MsgTypePublished    a b
      Subscribe     a b c         -> toJsonMsg3         MsgTypeSubscribe    a b c
      Subscribed    a b           -> toJsonMsg2         MsgTypeSubscribed   a b
      Unsubscribe   a b           -> toJsonMsg2         MsgTypeUnsubscribe  a b
      Unsubscribed  a             -> toJsonMsg1         MsgTypeUnsubscribed a
      Event         a b c d e     -> Array $ array <&>  MsgTypeEvent        <&> a <&> b <&> c <&> d <&> e

      Call          a b c d e     -> Array $ array <&>  MsgTypeCall         <&> a <&> b <&> c <&> d <&> e
      Result        a b c d       -> Array $ array <&>  MsgTypeResult       <&> a <&> b <&> c <&> d
      Register      a b c         -> toJsonMsg3         MsgTypeRegister     a b c
      Registered    a b           -> toJsonMsg2         MsgTypeRegistered   a b
      Unregister    a b           -> toJsonMsg2         MsgTypeUnregister   a b
      Unregistered  a             -> toJsonMsg1         MsgTypeUnregistered a
      Invocation    a b c d e     -> Array $ array <&>  MsgTypeInvocation   <&> a <&> b <&> c <&> d <&> e
      Yield         a b c d       -> Array $ array <&>  MsgTypeYield        <&> a <&> b <&> c <&> d


-- | Run toJSON on argument and append to Array.
(<&>) :: (ToJSON a) => Array -> a -> Array
v <&> x = v `V.snoc` (toJSON x)

-- | Run parseJSON on Array element.
(!) :: (FromJSON a) => Array -> Int -> Parser a
arr ! i = parseJSON (arr V.! i)

-- | Helper for optional arguments
opt :: (FromJSON a) => a -> Array -> Int -> Parser a
opt def arr i =
  case arr V.!? i of
    Nothing -> return def
    Just e  -> parseJSON e

emptyArgs :: Arguments
emptyArgs = Arguments array

emptyKwArgs :: ArgumentsKw
emptyKwArgs = ArgumentsKw dict


-- These make the ToJSON instance a bit more managable
toJsonMsg1 :: (ToJSON a) => MessageType -> a-> Value
toJsonMsg1 msgType a1 = Array $ array <&> msgType <&> a1

toJsonMsg2 :: (ToJSON a, ToJSON b) => MessageType -> a -> b -> Value
toJsonMsg2 msgType a1 a2 = Array $ array <&> msgType <&> a1 <&> a2

toJsonMsg3 :: (ToJSON b, ToJSON c, ToJSON d) => MessageType -> b -> c -> d -> Value
toJsonMsg3 msgType a1 a2 a3 = Array $ array <&> msgType <&> a1 <&> a2 <&> a3 

-- | Helper for constructing 'Error' messages
--
-- Sets empty 'Details', 'Arguments' and 'ArgumentsKw'. Wraps 'URI' in 'ErrorUri'.
defaultError :: MessageType -> ReqId -> ErrorUri -> Message
defaultError msgType reqId errorUri = 
  Error 
    msgType
    reqId
    (Details dict) 
    errorUri
    (Arguments array) 
    (ArgumentsKw dict)
