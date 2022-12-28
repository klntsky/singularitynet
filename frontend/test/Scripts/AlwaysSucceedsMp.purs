module Tests.Scripts.AlwaysSucceedsMp
  ( mkTrivialPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Data.Argonaut.Core (Json)
import Data.Bifunctor (bimap)
import Effect.Exception as Exception
import Utils (jsonReader)

-- | This is the trivial minting policy.
mkTrivialPolicy :: forall (r :: Row Type). Contract r MintingPolicy
mkTrivialPolicy = liftEither
  $ bimap (Exception.error <<< show) PlutusMintingPolicy
  $ jsonReader "script" _alwaysSucceedsMp

foreign import _alwaysSucceedsMp :: Json
