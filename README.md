# plutus-scaffold
This project contains the build systems and scripts needed to scaffold a plutus project quickly.

## Opening a shell

- `nix develop .#offchain`
- `nix develop .#onchain`
- `cabal repl --repl-options -Wwarn`

## Running
To get everything set up, copy the contents of this repo to your new project. You will then need to run the setup.sh script, which can take 0, 2, or 3 arguments.
* When given 0 arguments, it will prompt you to provide the project name, module name, and github URL. 
* When given 2 arguments, the first should be the project name, and the second should be the module name, e.g., `./setup.sh gero-gov GeroGov`. In this case, you will still be prompted for a github URL. 
* Finally, you may provide all three pieces of information at the command line, e.g., `./setup.sh gero-gov GeroGov https://github.com/mlabs-haskell/gero-gov`.

## Potential Issues
* If you submit your pull request, but get an error on the GitHub CIs saying something to the effect of "Binary cache mlabs doesn't exist or it's private," or that MLabs.cachix.com doesn't exist, then the cachix key is not setup, and you or whoever owns your repository will have to add that.
* If you get an error saying that "The package directory './.' does not contain any .cabal file," then you probably should either start with a fresh repository with no commits, or you should make a commit with git and rerun `nix-build nix/ci.nix`. This issue arises because nix is looking in your .git folder to try and identify what your .cabal file is, and since that has been renamed, nix seems to assume that there is no .cabal file. Starting with a fresh repo causes it to search the directory for your .cabal file, and making a commit changes the file name in the .git folder.

## Nix cache

You must have the following in your nix.conf:
```
substituters = https://public-plutonomicon.cachix.org https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=
```

For MLabs cache:
```
cachix authtoken eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJjJDI5NjUiLCJqdGkiOiIyNDc3MmJiMC0yYmFhLTQwMzItYjViNi03YTFlNjkwZDVlZDgiLCJzY29wZXMiOiJjYWNoZSJ9.UIE0NrDx8Xt3mkQY9GIw3Orz7HmXAU7A9t7dnpjXmiU
cachix use mlabs
```

### Troubleshooting

See: [Nix cache tips / troubleshooting](https://mlabs.slab.com/posts/mlabs-cachix-key-o6sx2nrm#h89kq-nix-cache-tips-troubleshooting)
