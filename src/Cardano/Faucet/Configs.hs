module Cardano.Faucet.Configs 
  ( AppConfig(..)
  , HttpConfig(..)
  , ExecutionConfig(..)
  , Secrets(..)
  , NodeConfig(..)
  , loadAppConfig
  ) where

import RIO

import Dhall

import qualified Data.Text as T

import System.Logging.Hlog (LoggingConfig)
import SubmitAPI.Config    (TxAssemblyConfig, DefaultChangeAddress)
import Explorer.Config     (ExplorerConfig)
import WalletAPI.TrustStore

import Cardano.Faucet.Types (DripAsset(..))

data AppConfig = AppConfig
  { httpConfig       :: HttpConfig
  , executionConfigs :: [ExecutionConfig]
  , loggingConfig    :: LoggingConfig
  , explorerConfig   :: ExplorerConfig
  , txAssemblyConfig :: TxAssemblyConfig
  , nodeConfig       :: NodeConfig
  , secrets          :: Secrets
  }
  deriving (Generic, FromDhall)

data HttpConfig = HttpConfig
  { getHost :: String,
    getPort :: Natural
  }
  deriving (Generic, FromDhall)

data ExecutionConfig = ExecutionConfig
  { maxOutputsPerTx    :: Natural
  , dripAsset          :: DripAsset
  , dripAmount         :: Natural
  , dripAmountLovelace :: Natural
  , changeAddr         :: DefaultChangeAddress
  , delay              :: Natural
  , queueSize          :: Natural
  }
  deriving (Generic, FromDhall)

data NodeConfig = NodeConfig
  { nodeSocketPath :: FilePath
  }
  deriving (Generic, FromDhall)

data Secrets = Secrets
  { secretFile :: SecretFile
  , keyPass    :: KeyPass
  }
  deriving (Generic, FromDhall)

loadAppConfig :: MonadIO f => Maybe String -> f AppConfig
loadAppConfig maybePath = liftIO $ input auto path
  where path = T.pack $ fromMaybe "./config/config.dhall" maybePath
