module Scripts.ListNFT
  ( mkListNFTPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (toData)
import Contract.Scripts
  ( ClientError
  , MintingPolicy(PlutusMintingPolicy)
  , PlutusScript
  , applyArgs
  )
import Contract.Value (CurrencySymbol)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Bifunctor (rmap)
import Types (ScriptVersion(..), StakingType(Bonded, Unbonded))
import Utils (jsonReader)

-- | This is the parameterized minting policy. It still needs to receive a
-- `CurrencySymbol` to become a minting policy
listNFTPolicy
  :: StakingType -> ScriptVersion -> Either JsonDecodeError PlutusScript
listNFTPolicy Bonded Production = jsonReader "script" _bondedListNFT
listNFTPolicy Unbonded Production = jsonReader "script" _unbondedListNFT
listNFTPolicy Bonded DebugNoTimeChecks = jsonReader "script"
  _bondedListNFTNoTimeChecks
listNFTPolicy Unbonded DebugNoTimeChecks = jsonReader "script"
  _unbondedListNFTNoTimeChecks

-- | This function takes a `CurrencySymbol` and produces the `MintingPolicy` for
-- the list NFT
mkListNFTPolicy
  :: forall (r :: Row Type) (a :: Type)
   . StakingType
  -> ScriptVersion
  -> CurrencySymbol
  -> Contract r (Either ClientError MintingPolicy)
mkListNFTPolicy st sv nftCs = do
  unappliedScript <- liftContractE $ listNFTPolicy st sv
  map (rmap PlutusMintingPolicy) $ applyArgs unappliedScript [ toData nftCs ]

foreign import _bondedListNFT :: Json
foreign import _unbondedListNFT :: Json
foreign import _bondedListNFTNoTimeChecks :: Json
foreign import _unbondedListNFTNoTimeChecks :: Json
