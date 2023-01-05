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
import Types (StakingType(Bonded, Unbonded))
import Utils (jsonReader)

-- | This is the parameterized minting policy. It still needs to receive a
-- `CurrencySymbol` to become a minting policy
listNFTPolicy :: StakingType -> Either JsonDecodeError PlutusScript
listNFTPolicy Bonded = jsonReader "script" _bondedListNFT
listNFTPolicy Unbonded = jsonReader "script" _unbondedListNFT

-- | This function takes a `CurrencySymbol` and produces the `MintingPolicy` for
-- the list NFT
mkListNFTPolicy
  :: forall (r :: Row Type) (a :: Type)
   . StakingType
  -> CurrencySymbol
  -> Contract r (Either ApplyArgsError MintingPolicy)
mkListNFTPolicy st nftCs = do
  unappliedScript <- liftContractE $ listNFTPolicy st
  pure <<< rmap PlutusMintingPolicy $ applyArgs unappliedScript [ toData nftCs ]

foreign import _bondedListNFT :: Json
foreign import _unbondedListNFT :: Json
