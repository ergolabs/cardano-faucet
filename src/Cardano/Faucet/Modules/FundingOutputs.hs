module Cardano.Faucet.Modules.FundingOutputs where

import RIO hiding (writeChan, readChan, newChan)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson (encode, decode)

import Control.Monad.Trans.Resource (MonadResource)

import System.Logging.Hlog (Logging(Logging, debugM), MakeLogging(..))

import CardanoTx.Models
import Cardano.Faucet.Types (DripAsset(..))

import qualified Database.RocksDB as Rocks

data FundingOutputs m = FundingOutputs
  { putOutput  :: DripAsset -> FullTxOut -> m ()
  , getOutput  :: DripAsset -> m (Maybe FullTxOut)
  , dropOutput :: DripAsset -> m ()
  }

mkFundingOutputs
  :: (MonadIO f, MonadResource f, MonadIO m)
  => Rocks.DB
  -> MakeLogging f m
  -> f (FundingOutputs m)
mkFundingOutputs db MakeLogging{..} = do
  logging <- forComponent "FundingOutputs"
  pure $ attachTracing logging FundingOutputs
    { putOutput  = putOutput' db
    , getOutput  = getOutput' db
    , dropOutput = dropOutput' db
    }

putOutput' :: MonadIO m => Rocks.DB -> DripAsset -> FullTxOut -> m ()
putOutput' db asset out = liftIO $ Rocks.put db (asKey asset) (LBS.toStrict $ encode out)

getOutput' :: MonadIO m => Rocks.DB -> DripAsset -> m (Maybe FullTxOut)
getOutput'  db asset = liftIO $ Rocks.get db (asKey asset) <&> (>>= (decode . LBS.fromStrict))

dropOutput' :: MonadIO m => Rocks.DB -> DripAsset -> m ()
dropOutput' db asset = liftIO . Rocks.delete db $ asKey asset

asKey :: Show a => a -> ByteString
asKey = BS.pack . show

attachTracing :: Monad m => Logging m -> FundingOutputs m -> FundingOutputs m
attachTracing Logging{..} FundingOutputs{..} =
  FundingOutputs
    { putOutput = \asset out -> do
        debugM $ "putOutput " <> show asset <> " " <> show out
        r <- putOutput asset out
        debugM $ "putOutput " <> show asset <> " " <> show out <> " -> " <> show r
        pure r
    , getOutput = \asset -> do
        debugM $ "getOutput " <> show asset
        r <- getOutput asset
        debugM $ "getOutput " <> show asset <> " -> " <> show r
        pure r
    , dropOutput = \asset -> do
        debugM $ "dropOutput " <> show asset
        r <- dropOutput asset
        debugM $ "dropOutput " <> show asset <> " -> " <> show r
        pure r
    }
