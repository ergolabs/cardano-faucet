let
  packages = import ./.;
  inherit (packages) pkgs cardano-faucet;
  inherit (cardano-faucet) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with cardano-faucet; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];

    buildInputs = with cardano-faucet; [
      pkgs.rocksdb
    ];
  }
