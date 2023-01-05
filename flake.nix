{
  description = "singularitynet";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]singularitynet \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    nixpkgs.follows = "plutarch/nixpkgs";
    haskell-nix.follows = "plutarch/haskell-nix";

    plutip.url = "github:mlabs-haskell/plutip?rev=0b92bb7b913d213457713c09bacae06110c47bac";

    plutarch.url = "github:CardaxDEX/plutarch?rev=e5a50283a0cb01ce1fee880943becda1ac19f3a0";
    plutarch.inputs.haskell-nix.follows = "plutip/haskell-nix";
    plutarch.inputs.nixpkgs.follows = "plutip/nixpkgs";

    ctl = {
      type = "github";
      owner = "Plutonomicon";
      repo = "cardano-transaction-lib";
      # NOTE
      # Keep this in sync with the rev in `frontend/packages.dhall`
      rev = "362c651cc9af7d40e2f8e4054a58fd209e81d2c3";
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
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay (import "${plutip.inputs.iohk-nix}/overlays/crypto") ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs {
        inherit system;
        overlays = [
           ctl.overlays.purescript
           ctl.overlays.runtime
           ctl.overlays.spago
           ];
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
            src = ./.;
            compiler-nix-name = ghcVersion;
            cabalProjectFileName = "cabal.project.onchain";
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

              shellHook = ''
                export NIX_SHELL_TARGET="onchain"
                ln -fs cabal.project.onchain cabal.project
              '';
            };
          };
      };

      frontend = {
        projectFor = system:
          let
            pkgs = nixpkgsFor' system;
            src = ./frontend;
            project = pkgs.purescriptProject {
              inherit src;
              projectName = "singularitynet-frontend";
              packageJson = ./frontend/package.json;
              packageLock = ./frontend/package-lock.json;
              nodejs = pkgs.nodejs-14_x;
              shell.packages = [ pkgs.fd ];
              strictComp = false;
            };
          in
          {
            flake = {
              packages = {
                frontend-bundle-web = project.bundlePursProject {
                  main = "Main";
                };
              };

              apps = {
                frontend-runtime = pkgs.launchCtlRuntime {
                  network = {
                    name = "preprod";
                    magic = 1; # use `null` for mainnet
                  };
                  node.tag = "1.35.3";
                };
              };

              checks = {
                frontend = project.runPlutipTest {
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

      frontend = {
        flake = perSystem (system: (frontend.projectFor system).flake);
      };

      packages = perSystem (system:
        self.onchain.flake.${system}.packages
        // self.frontend.flake.${system}.packages
      );

      apps = perSystem (system: self.frontend.flake.${system}.apps);

      checks = perSystem (system:
        self.onchain.flake.${system}.checks
        // self.frontend.flake.${system}.checks # includes formatting check as well
        # FIXME
        # Fourmolu from haskell.nix is broken, it might be possible to use one
        # from `nixpkgs` instead
        #
        # // {
        #   formatCheck = formatCheckFor system;
        # }
      );

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            checksss =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system}
              ++ [
                self.devShells.${system}.onchain.inputDerivation
                self.devShells.${system}.frontend.inputDerivation
              ];
          } ''
          echo $checksss
          touch $out
        ''
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        frontend = self.frontend.flake.${system}.devShell;
      });

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
