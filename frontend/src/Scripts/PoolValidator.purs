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
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Bifunctor (rmap)
import Scripts.Production (_bondedPoolValidator, _unbondedPoolValidator)
import Scripts.Debug
  ( _bondedPoolValidatorNoTimeChecks
  , _unbondedPoolValidatorNoTimeChecks
  )
import Types (BondedPoolParams, ScriptVersion(..))
import UnbondedStaking.Types (UnbondedPoolParams)
import Utils (jsonReader)

-- | This is the parameterized validator script. It still needs to receive a
-- `BondedPoolParams` to become a minting policy
bondedPoolValidator :: ScriptVersion -> Either JsonDecodeError PlutusScript
bondedPoolValidator Production = jsonReader "script" _bondedPoolValidator
bondedPoolValidator DebugNoTimeChecks = jsonReader "script"
  _bondedPoolValidatorNoTimeChecks

unbondedPoolValidator :: ScriptVersion -> Either JsonDecodeError PlutusScript
unbondedPoolValidator Production = jsonReader "script" _unbondedPoolValidator
unbondedPoolValidator DebugNoTimeChecks = jsonReader "script"
  _unbondedPoolValidatorNoTimeChecks

-- | This function takes a `BondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkBondedPoolValidator
  :: forall (r :: Row Type)
   . BondedPoolParams
  -> ScriptVersion
  -> Contract r (Either ApplyArgsError Validator)
mkBondedPoolValidator bpp sv = mkValidator (bondedPoolValidator sv) bpp

-- | This function takes a `UnbondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkUnbondedPoolValidator
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> ScriptVersion
  -> Contract r (Either ApplyArgsError Validator)
mkUnbondedPoolValidator ubp sv = mkValidator (unbondedPoolValidator sv) ubp

mkValidator
  :: forall (a :: Type) (r :: Row Type)
   . ToData a
  => Either JsonDecodeError PlutusScript
  -> a
  -> Contract r (Either ApplyArgsError Validator)
mkValidator ps params = do
  unappliedScript <- liftedE $ pure ps
  pure <<< rmap Validator $ applyArgs unappliedScript [ toData params ]
