#!/usr/bin/env bash
cabal run serialise -- state-policy -o ../frontend/src/Scripts/StateNFT.js
cabal run serialise -- list-policy -o ../frontend/src/Scripts/ListNFT.js
cabal run serialise -- validator -o ../frontend/src/Scripts/Production.js
cabal run -f "-TimeChecks" serialise -- validator -o ../frontend/src/Scripts/Debug.js
