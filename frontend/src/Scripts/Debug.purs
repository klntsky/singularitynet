module Scripts.Debug
  ( _bondedPoolValidatorNoTimeChecks
  , _unbondedPoolValidatorNoTimeChecks
  ) where

import Data.Argonaut.Core (Json)

_bondedPoolValidatorNoTimeChecks :: Json
_bondedPoolValidatorNoTimeChecks = _bondedPoolValidator

_unbondedPoolValidatorNoTimeChecks :: Json
_unbondedPoolValidatorNoTimeChecks = _unbondedPoolValidator

foreign import _bondedPoolValidator :: Json
foreign import _unbondedPoolValidator :: Json
