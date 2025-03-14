module Scripts.ListNFT
  ( mkListNFTPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (toData)
import Contract.Scripts
  ( ApplyArgsError
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
  -> Contract r (Either ApplyArgsError MintingPolicy)
mkListNFTPolicy st sv nftCs = do
  unappliedScript <- liftContractE $ listNFTPolicy st sv
  pure <<< rmap PlutusMintingPolicy $ applyArgs unappliedScript [ toData nftCs ]

_bondedListNFTNoTimeChecks :: Json
_bondedListNFTNoTimeChecks = _listNFT

_unbondedListNFTNoTimeChecks :: Json
_unbondedListNFTNoTimeChecks = _listNFT

_bondedListNFT :: Json
_bondedListNFT = _listNFT

_unbondedListNFT :: Json
_unbondedListNFT = _listNFT

foreign import _listNFT :: Json
