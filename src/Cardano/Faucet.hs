module Cardano.Faucet
  ( runApp
  ) where

import RIO.List
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource (runResourceT)

import           Cardano.Faucet.Configs   (loadAppConfig)
import qualified Cardano.Faucet.AppWiring as AppWiring

runApp :: [String] -> IO ()
runApp args = do
  conf <- loadAppConfig $ headMaybe args
  app  <- runResourceT $ AppWiring.mkApp (UnliftIO id) conf
  AppWiring.runApp app
