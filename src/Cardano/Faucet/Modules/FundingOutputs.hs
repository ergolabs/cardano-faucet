module Cardano.Faucet.Modules.FundingOutputs where

import RIO hiding (writeChan, readChan, newChan)

import Control.Monad.Trans.Resource (MonadResource)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson (encode, decode)
import           GHC.Natural (naturalToInt)

import System.Logging.Hlog (Logging(Logging, debugM), MakeLogging(..))

import CardanoTx.Models
import Cardano.Faucet.Types (DripAsset(..))
import Cardano.Faucet.Configs (OutputsStoreConfig(..))

import qualified Database.RocksDB as LDB

data FundingOutputs m = FundingOutputs
  { putOutput  :: DripAsset -> FullTxOut -> m ()
  , getOutput  :: DripAsset -> m (Maybe FullTxOut)
  , dropOutput :: DripAsset -> m ()
  }

mkFundingOutputs
  :: (MonadIO f, MonadResource f, MonadIO m)
  => OutputsStoreConfig
  -> MakeLogging f m
  -> f (FundingOutputs m)
mkFundingOutputs OutputsStoreConfig{..} MakeLogging{..} = do
  logging <- forComponent "FundingOutputs"
  db      <- LDB.open storePath
              LDB.defaultOptions
                { LDB.createIfMissing = True
                }
  pure $ attachTracing logging FundingOutputs
    { putOutput  = putOutput' db LDB.defaultWriteOptions
    , getOutput  = getOutput' db LDB.defaultReadOptions 
    , dropOutput = dropOutput' db LDB.defaultWriteOptions
    }

putOutput' :: MonadIO m => LDB.DB -> LDB.WriteOptions -> DripAsset -> FullTxOut -> m ()
putOutput' db opts asset out = liftIO $ LDB.put db opts (asKey asset) (LBS.toStrict $ encode out)

getOutput' :: MonadIO m => LDB.DB -> LDB.ReadOptions -> DripAsset -> m (Maybe FullTxOut)
getOutput'  db opts asset = liftIO $ LDB.get db opts (asKey asset) <&> (>>= (decode . LBS.fromStrict))

dropOutput' :: MonadIO m => LDB.DB -> LDB.WriteOptions -> DripAsset -> m ()
dropOutput' db opts asset = liftIO . LDB.delete db opts $ asKey asset

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
