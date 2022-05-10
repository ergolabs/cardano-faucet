let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ httpConfig =
  { getHost = "0.0.0.0"
  , getPort = 8083
  }
, executionConfigs =
  [ { maxOutputsPerTx    = 10
    , dripAsset          = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da.ergoTestTokenB"
    , dripAmount         = 1000000
    , dripAmountLovelace = 1500000
    , changeAddr         = "addr_test1vr007v5nktnksje3gnm4aw4arwrkcl5rvvx4lwa3w8mtzxgf6c2nt"
    , delay              = 15000000
    , queueSize          = 4096
    }
  ]
, loggingConfig =
  { fileHandlers   = [fileHandlers "./logs/tracker.log" LogLevel.Debug]
  , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
  }
, explorerConfig =
  { explorerUri = "https://testnet-api.quickblue.io"
  }
, txAssemblyConfig =
  { feePolicy         = FeePolicy.Balance
  , collateralPolicy  = CollateralPolicy.Cover
  , deafultChangeAddr = "addr_test1vr007v5nktnksje3gnm4aw4arwrkcl5rvvx4lwa3w8mtzxgf6c2nt"
  }
, nodeConfig =
  { nodeSocketPath = "/home/dex/cardano-node/testnet-node-local/bin/state-node-testnet/node.socket"
  }
, walletConfig =
  { secretFile   = "/home/dex/cardano-dex-backend/executor/ts.json"
  , keyPass      = "secret"
  , cardanoStyle = False
  }
, outputStoreConfig =
  { storePath = "./data/output-store"
  , cacheSize = 256
  }
, reCaptchaSecret = "secret"
}