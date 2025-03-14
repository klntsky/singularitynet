module Scripts.StateNFT
  ( mkStateNFTPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (toData)
import Contract.Scripts
  ( ApplyArgsError
  , MintingPolicy(PlutusMintingPolicy)
  , PlutusScript
  , applyArgs
  )
import Contract.Transaction (TransactionInput)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Bifunctor (rmap)
import Types (ScriptVersion(..), StakingType(Bonded, Unbonded))
import Utils (jsonReader)

-- | This is the parameterized minting policy. It still needs to receive a
-- `TransactionInput` to become a minting policy
nftPolicy :: StakingType -> ScriptVersion -> Either JsonDecodeError PlutusScript
nftPolicy Bonded Production = jsonReader "script" _bondedStateNFT
nftPolicy Unbonded Production = jsonReader "script" _unbondedStateNFT
nftPolicy Bonded DebugNoTimeChecks = jsonReader "script"
  _bondedStateNFTNoTimeChecks
nftPolicy Unbonded DebugNoTimeChecks = jsonReader "script"
  _unbondedStateNFTNoTimeChecks

-- | This function takes a `TransactionInput` and produces the `MintingPolicy` for
-- the state NFT
mkStateNFTPolicy
  :: forall (r :: Row Type) (a :: Type)
   . StakingType
  -> ScriptVersion
  -> TransactionInput
  -> Contract r (Either ApplyArgsError MintingPolicy)
mkStateNFTPolicy st sv txInput = do
  unappliedScript <- liftedE $ pure $ nftPolicy st sv
  pure <<< rmap PlutusMintingPolicy $ applyArgs unappliedScript
    [ toData txInput ]

_bondedStateNFTNoTimeChecks :: Json
_bondedStateNFTNoTimeChecks = _bondedStateNFT

_unbondedStateNFTNoTimeChecks :: Json
_unbondedStateNFTNoTimeChecks = _unbondedStateNFT

foreign import _bondedStateNFT :: Json
foreign import _unbondedStateNFT :: Json
