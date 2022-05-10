module Cardano.Faucet.Models
  ( DripOption(..)
  , DripRequest(..)
  , AckUtxo(..)
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson
import           Data.List.Split
import qualified Data.Text as T
import           Data.String

import Plutus.V1.Ledger.Tx (TxOutRef (TxOutRef))
import Cardano.Faucet.Types (DripAddress(..), DripAsset(..), ReCaptchaToken)

data DripOption = DripOption
  { dripAsset  :: DripAsset
  , dripAmount :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving ToJSON

data DripRequest = DripRequest
  { requestAddress :: DripAddress
  , requestAsset   :: DripAsset
  , reCaptchaToken :: ReCaptchaToken
  }
  deriving stock (Eq, Show, Generic)
  deriving FromJSON

newtype AckUtxo = AckUtxo
  { txOutRef :: TxOutRef
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AckUtxo where
  parseJSON (String s) = case splitOn ":" (T.unpack s) of
    [txId, idx] -> pure . AckUtxo $ TxOutRef (fromString txId) (read idx) -- todo: unsafe fromString
    _           -> fail "Invalid TxOutRef format"
  parseJSON _ = fail "Expected a string"
