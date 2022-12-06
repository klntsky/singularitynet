module Test.Main (main) where

import Contract.Prelude

--import Contract.Test.Plutip (PlutipConfig, testPlutipContracts)
--import Mote (MoteT)
--
--plutipCfg :: PlutipConfig
--plutipCfg = undefined
--
--suite :: MoteT Aff (Aff Unit) Aff
--suite = testPlutipContracts config do
--    undefined

main :: Effect Unit
main = log "Hello Purescript!"
