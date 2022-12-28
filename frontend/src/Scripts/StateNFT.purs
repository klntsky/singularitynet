module Scripts.StateNFT
  ( mkStateNFTPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (toData)
import Contract.Scripts
  ( ClientError
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
  -> Contract r (Either ClientError MintingPolicy)
mkStateNFTPolicy st sv txInput = do
  unappliedScript <- liftedE $ pure $ nftPolicy st sv
  map (rmap PlutusMintingPolicy) $ applyArgs unappliedScript [ toData txInput ]

foreign import _bondedStateNFT :: Json
foreign import _unbondedStateNFT :: Json
foreign import _bondedStateNFTNoTimeChecks :: Json
foreign import _unbondedStateNFTNoTimeChecks :: Json
