module Cardano.Faucet.AppWiring
  ( App(..)
  , mkApp
  ) where

import RIO

import GHC.Num (naturalToInt)

import Control.Concurrent (forkIO)
import Control.Monad.Trans.Resource
import System.Logging.Hlog (makeLogging, MakeLogging (forComponent), Logging (Logging, infoM))

import qualified Data.Map as Map

import qualified Cardano.Api as C

import Explorer.Service (mkExplorer)
import WalletAPI.TrustStore (mkTrustStoreUnsafe, mkTrustStore, SecretFile (unSigningKeyFile))
import WalletAPI.Vault (mkVault)
import WalletAPI.Utxos (WalletOutputs(..))
import NetworkAPI.Types (SocketPath(SocketPath))
import NetworkAPI.Node.Service (mkNetwork)
import SubmitAPI.Service (mkTransactions)

import Cardano.Faucet.Modules.RequestQueue
import Cardano.Faucet.Modules.FundingOutputs
import Cardano.Faucet.Modules.OutputResolver
import Cardano.Faucet.Configs (AppConfig(..), ExecutionConfig(..), WalletConfig(..), NodeConfig(..))
import Cardano.Faucet.Processes.Executor (mkExecutor, Executor (runExecutor))
import Cardano.Faucet.Http.Service (mkFaucet)
import Cardano.Faucet.Http.Server (runHttpServer)
import Cardano.Faucet.Services.ReCaptcha (mkReCaptcha)

import qualified Database.RocksDB as Rocks

newtype App m = App { runApp :: m () }

mkApp
  :: UnliftIO IO
  -> AppConfig
  -> Rocks.DB
  -> ResIO (App IO)
mkApp ul appconf@AppConfig{walletConfig=WalletConfig{..}, ..} db = do
  mkLogging  <- makeLogging loggingConfig
  fundOuts   <- mkFundingOutputs db mkLogging
  trustStore <-
    if cardanoStyle
      then mkTrustStoreUnsafe C.AsPaymentKey (unSigningKeyFile secretFile)
      else pure $ mkTrustStore C.AsPaymentKey secretFile
  let
    explorer      = mkExplorer explorerConfig
    vault         = mkVault trustStore keyPass
    walletOutputs = noopWalletOutputs
    sockPath      = SocketPath $ nodeSocketPath nodeConfig
    network       = mkNetwork C.AlonzoEra epochSlots testnet sockPath
    transactions  = mkTransactions network testnet walletOutputs vault txAssemblyConfig

    mkExecutorFor conf@ExecutionConfig{queueSize, dripAsset} = do
      rq   <- mkRequestQueue $ naturalToInt queueSize
      exec <- mkExecutor mkLogging conf rq fundOuts transactions
      pure (dripAsset, rq, exec)

  resolver <- mkOutputResolver explorer mkLogging
  modules  <- mapM mkExecutorFor executionConfigs
  let
    queues    = Map.fromList $ modules <&> (\(a, q, _) -> (a, q))
    executors = modules <&> (\(_, _, exec) -> exec)

  reCaptcha <- mkReCaptcha reCaptchaSecret mkLogging
  faucet    <- mkFaucet queues fundOuts resolver reCaptcha mkLogging appconf
  let runServer = runHttpServer httpConfig faucet ul
  Logging{infoM} <- forComponent mkLogging "App"

  pure . App $ infoM @String "Starting Faucet App .."
    >> mapM_ (forkIO . runExecutor) executors
    >> runServer

testnet :: C.NetworkId
testnet = C.Testnet $ C.NetworkMagic 1097911063

epochSlots :: C.ConsensusModeParams C.CardanoMode
epochSlots = C.CardanoModeParams $ C.EpochSlots 21600

noopWalletOutputs :: Applicative m => WalletOutputs m
noopWalletOutputs = WalletOutputs
  { selectUtxos       = const $ pure Nothing
  , selectUtxosStrict = const $ pure Nothing
  }
