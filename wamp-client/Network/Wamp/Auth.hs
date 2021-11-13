module Network.Wamp.Auth where

import qualified Data.ByteString.Base16 as BSB16
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Network.Wamp.Cryptosign as Cryptosign
import qualified Crypto.Sign.Ed25519 as Ed

import Data.Aeson
import Data.Aeson.Types

import           Network.Wamp.Messages
import           Network.Wamp.Types

type AuthId = T.Text
type SecretKey = T.Text

data Auth = AuthAnonymous
          | AuthCryptosign AuthId SecretKey
          deriving(Eq,Show)

computeAuthExtra :: Auth -> Either String [Pair]
computeAuthExtra AuthAnonymous = Right $ ["authmethods" .= toJSON ["anonymous"::T.Text]]
computeAuthExtra (AuthCryptosign authId secretKey) =
  Cryptosign.computeAuthExtra authId <$> Cryptosign.extractSecretKey secretKey

processChallenge AuthAnonymous _ = Left "CHALLENGE cannot be processed with anonymous auth method"
processChallenge (AuthCryptosign _ secretKey)  msg =
  case msg of
    Challenge (AuthMethod "cryptosign") e -> do
      k <- Cryptosign.extractSecretKey secretKey
      Cryptosign.processChallenge k e
    _ -> Left "auth method mismatch"
