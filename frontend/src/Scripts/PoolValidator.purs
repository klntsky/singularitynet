module Scripts.PoolValidator
  ( mkUnbondedPoolValidator
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
import Scripts.Production (_unbondedPoolValidator)
import Scripts.Debug
  ( _unbondedPoolValidatorNoTimeChecks
  )
import Types (ScriptVersion(..))
import UnbondedStaking.Types (UnbondedPoolParams)
import Utils (jsonReader)

unbondedPoolValidator :: ScriptVersion -> Either JsonDecodeError PlutusScript
unbondedPoolValidator Production = jsonReader "script" _unbondedPoolValidator
unbondedPoolValidator DebugNoTimeChecks = jsonReader "script"
  _unbondedPoolValidatorNoTimeChecks

-- | This function takes a `UnbondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkUnbondedPoolValidator
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> ScriptVersion
  -> Contract (Either ApplyArgsError Validator)
mkUnbondedPoolValidator ubp sv = mkValidator (unbondedPoolValidator sv) ubp

mkValidator
  :: forall (a :: Type) (r :: Row Type)
   . ToData a
  => Either JsonDecodeError PlutusScript
  -> a
  -> Contract (Either ApplyArgsError Validator)
mkValidator ps params = do
  unappliedScript <- liftedE $ pure ps
  pure <<< rmap Validator $ applyArgs unappliedScript [ toData params ]
