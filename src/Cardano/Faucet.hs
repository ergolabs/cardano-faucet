module Cardano.Faucet
  ( runApp
  ) where

import RIO.List
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource (runResourceT)

import Data.Default (def)

import           Cardano.Faucet.Configs (AppConfig(..), OutputsStoreConfig(..), loadAppConfig)
import qualified Cardano.Faucet.AppWiring as AppWiring

import qualified Database.RocksDB as Rocks

runApp :: [String] -> IO ()
runApp args = do
  conf@AppConfig{outputStoreConfig=OutputsStoreConfig{..}} <- loadAppConfig $ headMaybe args
  let
    dbconf = def { Rocks.createIfMissing = True }
    run db = do
      app <- runResourceT $ AppWiring.mkApp (UnliftIO id) conf db
      AppWiring.runApp app
  Rocks.withDB storePath dbconf run
