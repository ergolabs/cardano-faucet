module Cardano.Faucet.Types
  ( ReCaptchaSecret(..)
  , ReCaptchaToken(..)
  , DripAsset(..)
  , DripAddress(..)
  , dripAssetFromString
  ) where

import GHC.Generics

import           Data.Aeson
import           Data.List.Split
import qualified Data.Text as T
import           Data.String

import Dhall (FromDhall)
import qualified Dhall         as D
import Dhall.Core (Expr(..), Chunks(..))

import qualified Cardano.Api as C
import qualified Ledger.Tx.CardanoAPI as Interop
import Ledger (AssetClass, Address)
import Ledger.Value (assetClass, AssetClass (AssetClass))
import Plutus.V1.Ledger.Value (toString)

newtype ReCaptchaSecret = ReCaptchaSecret { getSecret :: String }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype FromDhall

newtype ReCaptchaToken = ReCaptchaToken { getToken :: String }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype FromJSON

newtype DripAsset = DripAsset { getDripAsset :: AssetClass }
  deriving stock (Eq, Ord, Generic)
  deriving newtype ToJSON

instance Show DripAsset where
  show (DripAsset (AssetClass (cs, tn))) = show cs <> assetClassSep <> toString tn

instance FromJSON DripAsset where
  parseJSON (String s) = either fail pure $ dripAssetFromString $ T.unpack s
  parseJSON _          = fail "Expected a string"

instance FromDhall DripAsset where
  autoWith _ = D.Decoder{..}
    where
      extract (TextLit (Chunks [] t)) = either (D.extractError . T.pack) pure $ dripAssetFromString $ T.unpack t
      extract expr                    = D.typeError expected expr

      expected = pure Text

assetClassSep :: String
assetClassSep = "."

dripAssetFromString :: String -> Either String DripAsset
dripAssetFromString s =
  fmap DripAsset
    $ case splitOn assetClassSep s of
      [policyId, tokenName] -> Right $ assetClass (fromString policyId) (fromString tokenName) -- todo: unsafe fromString
      _                     -> Left $ "AssetClass should be formatted as <policyId>" <> assetClassSep <> "<tokenName>"

newtype DripAddress = DripAddress { getDripAddress :: Address }
  deriving stock (Eq, Show, Ord, Generic)

instance FromJSON DripAddress where
  parseJSON (String s) =
    maybe
      (fail "Invalid Shelly Address")
      (pure . DripAddress)
      (do
        caddr <- C.deserialiseAddress C.AsShelleyAddress s
        either (const Nothing) pure
          (Interop.fromCardanoAddress (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) caddr)))
  parseJSON _ = fail "Expected a string"
