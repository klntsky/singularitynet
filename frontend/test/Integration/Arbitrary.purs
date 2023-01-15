module SNet.Test.Integration.Arbitrary (arbitraryInputs) where

import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.BigInt as BigInt
import Data.Tuple (uncurry)
import Test.QuickCheck.Gen (Gen, chooseInt, oneOf, vectorOf)
import SNet.Test.Integration.Types
  ( StateMachineInputs(..)
  , UserCommand(..)
  , AdminCommand(..)
  , InputConfig(..)
  )

arbitraryUserCommand :: Int -> Int -> Gen UserCommand
arbitraryUserCommand minStake maxStake =
  oneOf $ NonEmpty.cons'
    ( UserStake <<< BigInt.fromInt <$> chooseInt
        minStake
        maxStake
    )
    [ pure UserWithdraw
    , pure DoNothing
    ]

arbitraryAdminCommand :: Int -> Int -> Gen AdminCommand
arbitraryAdminCommand minDeposit maxDeposit = oneOf $ NonEmpty.cons'
  (AdminDeposit <<< BigInt.fromInt <$> chooseInt minDeposit maxDeposit)
  [ pure <<< AdminDeposit <<< BigInt.fromInt $ 0
  , pure AdminClose
  ]

arbitraryInputs :: InputConfig -> Gen StateMachineInputs
arbitraryInputs (InputConfig cfg) = do
  userInputs :: Array (Array (Array UserCommand)) <-
    vectorOf cfg.nCycles
      $ vectorOf cfg.nUsers do
          nActions <- chooseInt 1 cfg.maxUserActionsPerCycle
          vectorOf nActions $
            uncurry arbitraryUserCommand cfg.stakeRange
  adminInput :: Array AdminCommand <- vectorOf cfg.nCycles $
    uncurry arbitraryAdminCommand cfg.depositRange
  pure $ StateMachineInputs { userInputs, adminInput }

