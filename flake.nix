{
  description = "singularitynet";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]singularitynet \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";

    plutip.url = "github:mlabs-haskell/plutip?rev=88d069d68c41bfd31b2057446a9d4e584a4d2f32";

    plutarch.url = "github:CardaxDEX/plutarch?rev=e5a50283a0cb01ce1fee880943becda1ac19f3a0";
    plutarch.inputs.haskell-nix.follows = "plutip/haskell-nix";
    plutarch.inputs.nixpkgs.follows = "plutip/nixpkgs";

    ctl = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      # NOTE
      # Keep this in sync with the rev in `frontend/packages.dhall`
      rev = "03e65b0ebc0be3ccbc98a0621ce786390f887129";
    };
  };


  outputs =
    inputs@{ self
    , nixpkgs
    , haskell-nix
    , plutarch
    , plutip
    , ctl
    , ...
    }:
    let
      # GENERAL
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay (import "${plutip.inputs.iohk-nix}/overlays/crypto") ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs {
        inherit system;
        overlays = [ ctl.overlay.${system} ];
      };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [
              pkgs'.git
              pkgs'.fd
              pkgs'.haskellPackages.cabal-fmt
              pkgs'.nixpkgs-fmt
              (pkgs.haskell-nix.tools onchain.ghcVersion { inherit (plutarch.tools) fourmolu; }).fourmolu
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check
          mkdir $out
        ''
      ;

      deferPluginErrors = true;

      # ONCHAIN / Plutarch

      onchain = rec {
        ghcVersion = "ghc921";

        projectFor = system:
          let pkgs = nixpkgsFor system; in
          let pkgs' = nixpkgsFor' system; in
          (nixpkgsFor system).haskell-nix.cabalProject' {
            src = ./onchain;
            compiler-nix-name = ghcVersion;
            inherit (plutarch) cabalProjectLocal;
            extraSources = plutarch.extraSources ++ [
              {
                src = inputs.plutarch;
                subdirs = [ "." ];
              }
            ];
            modules = [ (plutarch.haskellModule system) ];
            shell = {
              withHoogle = true;

              exactDeps = true;

              # We use the ones from Nixpkgs, since they are cached reliably.
              # Eventually we will probably want to build these with haskell.nix.
              nativeBuildInputs = [
                pkgs'.cabal-install
                pkgs'.fd
                pkgs'.haskellPackages.apply-refact
                pkgs'.haskellPackages.cabal-fmt
                pkgs'.hlint
                pkgs'.nixpkgs-fmt
              ];

              inherit (plutarch) tools;

              additional = ps: [
                ps.plutarch
                ps.tasty-quickcheck
              ];
            };
          };
      };

      # OFFCHAIN / Testnet, Cardano, ...

      offchain = rec {
        ghcVersion = "ghc8107";

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
            plutipin = inputs.plutip.inputs;
            fourmolu = pkgs.haskell-nix.tool "ghc921" "fourmolu" { };
            project = pkgs.haskell-nix.cabalProject' {
              src = ./offchain;
              compiler-nix-name = ghcVersion;
              inherit (plutip) cabalProjectLocal;
              extraSources = plutip.extraSources ++ [
                {
                  src = "${plutip}";
                  subdirs = [ "." ];
                }
              ];
              modules = [
                ({ config, ... }: {
                  packages.singularitynet-offchain.components.tests.singularitynet-offchain-test.build-tools = [
                    project.hsPkgs.cardano-cli.components.exes.cardano-cli
                    project.hsPkgs.cardano-node.components.exes.cardano-node
                  ];

                })
              ] ++ plutip.haskellModules;

              shell = {
                withHoogle = true;

                exactDeps = true;

                # We use the ones from Nixpkgs, since they are cached reliably.
                # Eventually we will probably want to build these with haskell.nix.
                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.fd
                  pkgs'.haskellPackages.apply-refact
                  pkgs'.haskellPackages.cabal-fmt
                  pkgs'.hlint
                  pkgs'.nixpkgs-fmt

                  project.hsPkgs.cardano-cli.components.exes.cardano-cli
                  project.hsPkgs.cardano-node.components.exes.cardano-node

                  fourmolu
                ];

                tools.haskell-language-server = { };

                additional = ps: [ ps.plutip ];
              };
            };
          in
          project;
      };

      frontend = {
        projectFor = system:
          let
            pkgs = nixpkgsFor' system;
            src = ./frontend;
            project = pkgs.purescriptProject {
              inherit src;
              projectName = "singularitynet-frontend";
              nodejs = pkgs.nodejs-12_x;
            };
          in
          {
            flake = {
              packages = {
                frontend-bundle-web = project.bundlePursProject {
                  sources = [ "src" "exe" ];
                  main = "Main";
                  entrypoint = "main.js";
                };
              };

              apps = {
                frontend-runtime = pkgs.launchCtlRuntime { };
              };

              checks = {
                frontend = project.runPursTest {
                  name = "singularitynet-frontend";
                  sources = [ "src" "test" ];
                  testMain = "Test.Main";
                };

                format-check = pkgs.runCommand "formatting-check"
                  {
                    nativeBuildInputs = [
                      pkgs.easy-ps.purs-tidy
                      pkgs.fd
                    ];
                  }
                  ''
                    cd ${src}
                    purs-tidy check $(fd -epurs)
                    touch $out
                  '';
              };

              devShell = project.devShell;
            };
          };
      };

    in
    {
      inherit nixpkgsFor;

      onchain = {
        project = perSystem onchain.projectFor;
        flake = perSystem (system: (onchain.projectFor system).flake { });
      };

      offchain = {
        project = perSystem offchain.projectFor;
        flake = perSystem (system: (offchain.projectFor system).flake { });
      };

      frontend = {
        flake = perSystem (system: (frontend.projectFor system).flake);
      };

      packages = perSystem (system:
        self.onchain.flake.${system}.packages
        // self.offchain.flake.${system}.packages
        // self.frontend.flake.${system}.packages
      );

      apps = perSystem (system: self.frontend.flake.${system}.apps);

      checks = perSystem (system:
        self.onchain.flake.${system}.checks
        // self.offchain.flake.${system}.checks
        // self.frontend.flake.${system}.checks # includes formatting check as well
        // {
          formatCheck = formatCheckFor system;
        }
      );

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            checksss =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system}
              ++ [
                self.devShells.${system}.onchain.inputDerivation
                self.devShells.${system}.offchain.inputDerivation
                self.devShells.${system}.frontend.inputDerivation
              ];
          } ''
          echo $checksss
          touch $out
        ''
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        offchain = self.offchain.flake.${system}.devShell;
        frontend = self.frontend.flake.${system}.devShell;
      });
    };
}
