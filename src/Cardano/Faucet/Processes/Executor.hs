module Cardano.Faucet.Processes.Executor
  ( Executor(..)
  , mkExecutor
  ) where

import RIO

import           Data.Set as Set
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL

import System.Logging.Hlog (Logging(..), MakeLogging(..))
import GHC.Natural (naturalToInt)
import GHC.Num (naturalToInteger)

import Cardano.Faucet.Modules.RequestQueue
import Cardano.Faucet.Modules.FundingOutputs
import Cardano.Faucet.Models
import CardanoTx.Models
import CardanoTx.Interop
import Cardano.Faucet.Configs
import Cardano.Faucet.Types (DripAddress(getDripAddress), DripAsset(..))

import qualified Ledger as Interval
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Api (adaSymbol)
import           Plutus.V2.Ledger.Api (adaToken)
import           PlutusTx.Prelude (Group(inv))
import           Cardano.Api (Lovelace(Lovelace))

import SubmitAPI.Service
import SubmitAPI.Config (unwrapChangeAddress)

newtype Executor m = Executor
  { runExecutor :: m ()
  }

mkExecutor
  :: (Monad f, MonadIO m)
  => MakeLogging f m
  -> ExecutionConfig
  -> RequestQueue m
  -> FundingOutputs m
  -> Transactions m era
  -> f (Executor m)
mkExecutor MakeLogging{..} conf@ExecutionConfig{dripAsset=DripAsset(AssetClass (_, tn))} rq outs txs = do
  logging <- forComponent $ "Executor#" <> toString tn
  pure . Executor $ runExecution logging conf rq outs txs

runExecution
  :: MonadIO m
  => Logging m
  -> ExecutionConfig
  -> RequestQueue m
  -> FundingOutputs m
  -> Transactions m era
  -> m ()
runExecution Logging{..} conf@ExecutionConfig{..} RequestQueue{..} FundingOutputs{..} txs@Transactions{..} =
  forever $ do
    let
      collectRequests acc n =
        dequeue >>= \case
          Just r | n < maxOutputsPerTx -> collectRequests (r : acc) (n + 1)
          _                            -> pure acc
      reEnqueueRequests (r : rs) = enqueue r >>= (\success -> if success then reEnqueueRequests rs else pure ())
      reEnqueueRequests _        = pure ()

    fundingIn <- getOutput dripAsset
    reqs      <- collectRequests [] 0
    case (fundingIn, NEL.nonEmpty reqs) of
      (Nothing, _)                  -> infoM @String "Waiting for a funding UTxO .."
      (_, Nothing)                  -> infoM @String "Nothing to execute .."
      (Just fundingIn', Just reqs') -> do
        txres <- mkTx conf txs (mkPkhTxIn fundingIn') reqs'
        case txres of
          Just (txc, leftReqs) -> do
            debugM $ "Building " ++ show txc
            tx <- finalizeTx txc
            debugM $ "Submitting " ++ show tx
            txId <- submitTx tx -- todo: handle tx error
            debugM $ "Submitted Tx{txId=" <> show txId <> "}"

            let
              fundingAddr       = fullTxOutAddress fundingIn'
              nextFundingOutput = List.find ((== fundingAddr) . fullTxOutAddress) (extractCardanoTxOutputs tx)
            maybe (infoM @String "Next funding UTxO not found") (putOutput dripAsset) nextFundingOutput
            reEnqueueRequests leftReqs
          Nothing -> reEnqueueRequests reqs
    threadDelay $ naturalToInt delay

mkTx
  :: forall m era. Monad m
  => ExecutionConfig
  -> Transactions m era
  -> FullTxIn
  -> NonEmpty DripRequest
  -> m (Maybe (TxCandidate, [DripRequest]))
mkTx ExecutionConfig{..} Transactions{..} fundingIn requests' =
  let
    mkTx' :: [DripRequest] -> [DripRequest] -> m (Maybe (TxCandidate, [DripRequest]))
    mkTx' requests residue = do
      let
        mkCandidate DripRequest{..} = TxOutCandidate addr value EmptyDatum
          where
            addr  = getDripAddress requestAddress
            value =
              assetClassValue (getDripAsset dripAsset) (naturalToInteger dripAmount) <>
              assetClassValue adaAssetClass (naturalToInteger dripAmountLovelace)
        hasSufficientValue = (>= minSufficientLovelace) . flip assetClassValueOf adaAssetClass . txOutCandidateValue

        dripOutputs = requests <&> mkCandidate

        fundingAddr = fullTxOutAddress . fullTxInTxOut $ fundingIn
        initValue   = fullTxOutValue . fullTxInTxOut $ fundingIn
        givenValue  = RIO.fold $ dripOutputs <&> txOutCandidateValue

        preFundingOut = TxOutCandidate fundingAddr reducedValue EmptyDatum
          where reducedValue = initValue <> inv givenValue
        preTxc = TxCandidate
          { txCandidateInputs       = Set.fromList [fundingIn]
          , txCandidateOutputs      = preFundingOut : (requests <&> mkCandidate)
          , txCandidateValueMint    = mempty
          , txCandidateMintInputs   = mempty
          , txCandidateChangePolicy = Just . ReturnTo . unwrapChangeAddress $ changeAddr
          , txCandidateValidRange   = Interval.always
          , txCandidateSigners      = mempty
          }
      Lovelace fee <- estimateTxFee mempty preTxc <&> (<> txFeeCorrectionValue)
      let
        fundingOut = TxOutCandidate fundingAddr reducedValue EmptyDatum
          where reducedValue = initValue <> inv givenValue <> inv (assetClassValue adaAssetClass fee)
      if hasSufficientValue fundingOut
        then
          let
            txc = TxCandidate
              { txCandidateInputs       = Set.fromList [fundingIn]
              , txCandidateOutputs      = fundingOut : (requests <&> mkCandidate)
              , txCandidateValueMint    = mempty
              , txCandidateMintInputs   = mempty
              , txCandidateChangePolicy = Just . ReturnTo . unwrapChangeAddress $ changeAddr
              , txCandidateValidRange   = Interval.always
              , txCandidateSigners      = mempty
              }
          in pure $ Just (txc, residue)
        else
          case requests of
            r : rs -> mkTx' rs (r : residue)
            []     -> pure Nothing

  in mkTx' (RIO.toList requests') []

adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken

minSufficientLovelace :: Integer
minSufficientLovelace = 2000000

txFeeCorrectionValue :: Lovelace
txFeeCorrectionValue = 6512 -- magic
