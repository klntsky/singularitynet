module Scripts.PoolValidator
  ( mkBondedPoolValidator
  , mkUnbondedPoolValidator
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (class ToData, toData)
import Contract.Scripts
  ( ApplyArgsError
  , PlutusScript
  , Validator(Validator)
  , applyArgs
  )
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Bifunctor (rmap)
import Types (BondedPoolParams)
import UnbondedStaking.Types (UnbondedPoolParams)
import Utils (jsonReader)

-- | This is the parameterized validator script. It still needs to receive a
-- `BondedPoolParams` to become a minting policy
bondedPoolValidator :: Either JsonDecodeError PlutusScript
bondedPoolValidator = jsonReader "script" _bondedPoolValidator

unbondedPoolValidator :: Either JsonDecodeError PlutusScript
unbondedPoolValidator = jsonReader "script" _unbondedPoolValidator

-- | This function takes a `BondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkBondedPoolValidator
  :: forall (r :: Row Type)
   . BondedPoolParams
  -> Contract r (Either ApplyArgsError Validator)
mkBondedPoolValidator = mkValidator bondedPoolValidator

-- | This function takes a `UnbondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkUnbondedPoolValidator
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> Contract r (Either ApplyArgsError Validator)
mkUnbondedPoolValidator = mkValidator unbondedPoolValidator

mkValidator
  :: forall (a :: Type) (r :: Row Type)
   . ToData a
  => Either JsonDecodeError PlutusScript
  -> a
  -> Contract r (Either ApplyArgsError Validator)
mkValidator ps params = do
  unappliedScript <- liftedE $ pure ps
  pure <<< rmap Validator $ applyArgs unappliedScript [ toData params ]

foreign import _bondedPoolValidator :: Json
foreign import _unbondedPoolValidator :: Json
