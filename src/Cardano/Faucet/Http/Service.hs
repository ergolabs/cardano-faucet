module Cardano.Faucet.Http.Service
  ( Faucet(..)
  , mkFaucet
  ) where

import Control.Monad.Except

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

import Servant.Server (ServerError(..), err400, err500)

import Cardano.Faucet.Models
import Cardano.Faucet.Modules.RequestQueue
import Cardano.Faucet.Modules.OutputResolver
import Cardano.Faucet.Modules.FundingOutputs

import CardanoTx.Models ( FullTxOut(fullTxOutValue) )
import Ledger.Value (flattenValue, AssetClass (AssetClass))
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)

import Cardano.Faucet.Types (DripAsset(..))
import System.Logging.Hlog (MakeLogging(..), Logging (debugM, Logging))
import Cardano.Faucet.Configs
import RIO ((<&>))
import GHC.Natural (naturalToInteger)
import Cardano.Faucet.Services.ReCaptcha

type RequestQueues m   = Map.Map DripAsset (RequestQueue m)

data Faucet m = Faucet
  { registerRequest :: DripRequest -> ExceptT ServerError m ()
  , ackFundingUtxo  :: AckUtxo -> ExceptT ServerError m ()
  , getDripOptions  :: ExceptT ServerError m [DripOption]
  }

mkFaucet
  :: (Monad f, Monad m)
  => RequestQueues m
  -> FundingOutputs m
  -> OutputResolver m
  -> ReCaptcha m
  -> MakeLogging f m
  -> AppConfig
  -> f (Faucet m)
mkFaucet rqs fouts oresolver captcha MakeLogging{..} conf = do
  logging <- forComponent "Faucet"
  pure Faucet
    { registerRequest = traceMethod logging "registerRequest" $ registerRequestReCaptchaVerify captcha $ registerRequest' rqs
    , ackFundingUtxo  = traceMethod logging "ackFundingUtxo" $ ackFundingUtxo' oresolver fouts
    , getDripOptions  = getDripOptions' conf
    }

traceMethod
  :: (MonadTrans t, Monad m, Monad (t m), Show a, Show b)
  => Logging m
  -> String
  -> (a -> t m b)
  -> a
  -> t m b
traceMethod Logging{..} methodName fn req = do
  lift . debugM $ methodName <> " " <> show req
  r <- fn req
  lift . debugM $ methodName <> " " <> show req <> " -> " <> show r
  pure r

registerRequest' :: Monad m => RequestQueues m -> DripRequest -> ExceptT ServerError m ()
registerRequest' queues req@DripRequest{..} = -- todo: filter repeating addresses.
  case Map.lookup requestAsset queues of
    Just RequestQueue{..} ->
      (lift . enqueue $ req) >>= \case
        True -> pure ()
        _    -> throwError err500 { errBody = "Service overloaded. Try again later." }
    _ -> throwError err400 { errBody = "Requested asset isn't supported." }

registerRequestReCaptchaVerify
  :: Monad m
  => ReCaptcha m
  -> (DripRequest -> ExceptT ServerError m ())
  -> DripRequest
  -> ExceptT ServerError m ()
registerRequestReCaptchaVerify ReCaptcha{..} fa req@DripRequest{..} =
  lift (verify reCaptchaToken) >>= \case
    True -> fa req
    _    -> throwError err400 { errBody = "Invalid ReCaptcha token." }

ackFundingUtxo' :: Monad m => OutputResolver m -> FundingOutputs m -> AckUtxo -> ExceptT ServerError m ()
ackFundingUtxo' OutputResolver{..} FundingOutputs{..} (AckUtxo ref) =
  (lift . resolve $ ref) >>= \case
    Just out ->
      let
        assets = flattenValue . fullTxOutValue $ out

        selectAsset ((cs, tn, _) : _) | cs /= adaSymbol = Just $ AssetClass (cs, tn)
        selectAsset (_ : xs)                            = selectAsset xs
        selectAsset _                                   = Nothing

        asset = DripAsset $ fromMaybe (AssetClass (adaSymbol, adaToken)) $ selectAsset assets
      in lift $ putOutput asset out
    Nothing  -> throwError err400 { errBody = "UTxO not found." }

getDripOptions' :: Monad m => AppConfig -> ExceptT ServerError m [DripOption]
getDripOptions' AppConfig{executionConfigs} =
  pure $ executionConfigs <&> (\ExecutionConfig{dripAsset, dripAmount} -> DripOption dripAsset (naturalToInteger dripAmount))
