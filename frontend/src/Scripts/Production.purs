module Scripts.Production (_bondedPoolValidator, _unbondedPoolValidator) where

import Data.Argonaut.Core (Json)

foreign import _bondedPoolValidator :: Json
foreign import _unbondedPoolValidator :: Json
