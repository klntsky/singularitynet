module UnbondedStaking.UserStake (userStakeUnbondedPoolContract) where

import Contract.Prelude hiding (length)

import Contract.Address
  ( addressToBech32
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , throwContractError
  )
import Contract.Numeric.Natural (Natural, toBigInt)
import Contract.PlutusData
  ( PlutusData
  , Redeemer(Redeemer)
  , Datum(Datum)
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionOutput)
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustMintValueWithRedeemer
  , mustSpendScriptOutput
  , mustValidateIn
  )
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Value (Value, mkTokenName, singleton)
import Control.Applicative (unless)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Data.Array (head)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings
  ( unbondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import Types
  ( ListAction(ListInsert)
  , MintingAction(MintHead)
  , ScriptVersion
  , StakingType(Unbonded)
  )
import UnbondedStaking.Types
  ( Entry(Entry)
  , UnbondedStakingAction(StakeAct)
  , UnbondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  , UnbondedPoolParams(UnbondedPoolParams)
  )
import UnbondedStaking.Utils (getUserTime)
import Utils
  ( findInsertUpdateElem
  , getUtxoWithNFT
  , hashPkh
  , mkOnchainAssocList
  , logInfo_
  , repeatUntilConfirmed
  , mustPayToScript
  , getUtxoDatumHash
  )

-- Deposits a certain amount in the pool
userStakeUnbondedPoolContract
  :: UnbondedPoolParams
  -> ScriptVersion
  -> Natural
  -> Contract ()
       { txId :: String }
userStakeUnbondedPoolContract
  params@
    ( UnbondedPoolParams
        { minStake
        , maxStake
        , unbondedAssetClass
        , nftCs
        , assocListCs
        }
    )
  scriptVersion
  amt = repeatUntilConfirmed params confirmationTimeout submissionAttempts $ do
  -- Fetch information related to the pool
  -- Get network ID
  networkId <- getNetworkId
  userPkh <- liftedM "userStakeUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  logInfo_ "userStakeUnbondedPoolContract: User's PaymentPubKeyHash" userPkh

  -- Get utxos at the wallet address
  userUtxos <-
    liftedM "userStakeUnbondedPoolContract: Cannot get wallet's utxos" $
      getWalletUtxos
  userBech32 <-
    addressToBech32 =<<
      liftedM "userStakeUnbondedPoolContract: Could not get wallet address"
        getWalletAddress
  logInfo_ "userStakeUnbondedPoolContract: User's address" userBech32

  -- Get the unbonded pool validator and hash
  validator <- liftedE' "userStakeUnbondedPoolContract: Cannot create validator"
    $ mkUnbondedPoolValidator params scriptVersion
  let valHash = validatorHash validator
  logInfo_ "userStakeUnbondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash Nothing
  logInfo_ "userStakeUnbondedPoolContract: Pool address"
    $ fromPlutusAddress networkId poolAddr

  -- Get the unbonded pool's datum and utxos
  unbondedPoolUtxos <- utxosAt poolAddr
  logInfo_ "userStakeUnbondedPoolContract: Pool UTXOs" unbondedPoolUtxos
  tokenName <- liftContractM
    "userStakeUnbondedPoolContract: Cannot create TokenName"
    unbondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "userStakeUnbondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT unbondedPoolUtxos nftCs tokenName
  logInfo_ "userStakeUnbondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "userStakeUnbondedPoolContract: Could not get Pool UTXO's Datum Hash"
      $ getUtxoDatumHash poolTxOutput
  logInfo_ "userStakeUnbondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "userStakeUnbondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  unbondedStakingDatum :: UnbondedStakingDatum <-
    liftContractM
      "userStakeUnbondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)

  let
    amtBigInt = toBigInt amt
    assetDatum = Datum $ toData AssetDatum
    stateTokenValue = singleton nftCs tokenName one
    assetParams = unwrap unbondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName
    stakeValue = singleton assetCs assetTn amtBigInt

  -- Get the the hashed user PKH used as key in the entry list
  hashedUserPkh <- liftAff $ hashPkh userPkh
  logInfo_ "userStakeUnbondedPoolContract: Hashed user PKH" hashedUserPkh

  -- Get the minting policy and currency symbol from the list NFT:
  listPolicy <- liftedE $ mkListNFTPolicy Unbonded scriptVersion nftCs

  -- Get the token name for the user by hashing
  assocListTn <-
    liftContractM
      "userStakeUnbondedPoolContract: Could not create token name for user`"
      $ mkTokenName hashedUserPkh
  logInfo_ "userStakeUnbondedPoolContract: assoc list token name" assocListTn
  let entryValue = singleton assocListCs assocListTn one

  -- Get the staking range to use
  logInfo' "userStakeUnbondedPoolContract: Getting user time range..."
  { currTime, range } <- getUserTime params scriptVersion
  logInfo' $ "userStakeUnbondedPoolContract: Current time: " <> show currTime
  logInfo' $ "userStakeUnbondedPoolContract: TX Range" <> show range

  -- Figure out where in the linked list the new stake goes
  constraints /\ lookup <- case unbondedStakingDatum of
    StateDatum { maybeEntryName: Nothing, open: true } -> do
      logInfo'
        "userStakeUnbondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Nothing, open: true }"
      -- If the state UTXO has nothing, it is a head deposit, spending the state
      -- UTXO. It's the first stake so we just need to check amt is inside
      -- bounds.
      unless (minStake <= amt && amt <= maxStake)
        $ throwContractError
            "userStakeUnbondedPoolContract: Stake amount outside of min/max range"
      let
        mh = MintHead poolTxInput
        -- Minting a new Entry
        valRedeemer = Redeemer $ toData $ StakeAct
          { stakeAmount: amt
          , stakeHolder: userPkh
          , mintingAction: Just mh
          }
        mintRedeemer = Redeemer $ toData $ ListInsert mh
        -- Updated unbonded state datum
        unbondedStateDatum = Datum $ toData $ StateDatum
          { maybeEntryName: Just hashedUserPkh
          , open: true
          }
        -- The new Entry
        entryDatum = Datum $ toData $ EntryDatum
          { entry: Entry
              { key: hashedUserPkh
              , deposited: amtBigInt
              , newDeposit: amtBigInt
              , rewards: zero
              , totalRewards: zero
              , totalDeposited: zero
              , open: true
              , next: Nothing -- There are no other elements in the list
              }
          }

      let
        unbondedStateDatumLookup = ScriptLookups.datum unbondedStateDatum
        entryDatumLookup = ScriptLookups.datum entryDatum

        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustMintValueWithRedeemer mintRedeemer entryValue
            , mustPayToScript valHash unbondedStateDatum stateTokenValue
            , mustPayToScript valHash assetDatum stakeValue
            , mustPayToScript valHash entryDatum entryValue
            , mustBeSignedBy userPkh
            , mustSpendScriptOutput poolTxInput valRedeemer
            , mustValidateIn range
            ]

        lookup :: ScriptLookups.ScriptLookups PlutusData
        lookup = mconcat
          [ ScriptLookups.mintingPolicy listPolicy
          , ScriptLookups.validator validator
          , ScriptLookups.unspentOutputs userUtxos
          , ScriptLookups.unspentOutputs unbondedPoolUtxos
          , unbondedStateDatumLookup
          , entryDatumLookup
          ]
      pure $ constraints /\ lookup
    -- Here, the list is non empty:
    StateDatum { maybeEntryName: Just key, open: true } -> do
      logInfo'
        "userStakeUnbondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Just ..., open: true }"
      let assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos
      logInfo_ "userStakeUnbondedPoolContract: Assoc list keys" $ map fst
        assocList

      -- If hashedUserPkh < key, we have a head deposit, spending the state utxo
      -- If hashedUserPkh == key, this is a non-init deposit spending the first
      -- assoc. list element.
      -- If hashedUserPkh > key, we filter elements and find the last suitable
      -- one.
      case compare hashedUserPkh key of
        LT -> do
          logInfo' "userStakeUnbondedPoolContract: Compare LT"
          unless (minStake <= amt && amt <= maxStake)
            $ throwContractError
                "userStakeUnbondedPoolContract: Stake amount outside of min/max \
                \range"
          -- Minting a new Entry
          let
            mh = MintHead poolTxInput

            -- Minting a new Entry
            valRedeemer = Redeemer $ toData $ StakeAct
              { stakeAmount: amt
              , stakeHolder: userPkh
              , mintingAction: Just mh
              }
            mintRedeemer = Redeemer $ toData $ ListInsert mh
            -- Updated unbonded state datum
            unbondedStateDatum = Datum $ toData $ StateDatum
              { maybeEntryName: Just hashedUserPkh -- points to new head
              , open: true
              }
            -- The new Entry
            entryDatum = Datum $ toData $ EntryDatum
              { entry: Entry
                  { key: hashedUserPkh
                  , deposited: amtBigInt
                  , newDeposit: amtBigInt
                  , rewards: zero
                  , totalRewards: zero
                  , totalDeposited: zero
                  , open: true
                  , next: Just key -- points to the previous head.
                  }
              }

          logInfo_ "userStakeUnbondedPoolContract: Minting action" mh
          let unbondedStateDatumLookup = ScriptLookups.datum unbondedStateDatum
          let
            entryDatumLookup = ScriptLookups.datum entryDatum

            constraints :: TxConstraints Unit Unit
            constraints =
              mconcat
                [ mustMintValueWithRedeemer mintRedeemer entryValue
                , mustPayToScript valHash unbondedStateDatum stateTokenValue
                , mustPayToScript valHash assetDatum stakeValue
                , mustPayToScript valHash entryDatum entryValue
                , mustBeSignedBy userPkh
                , mustSpendScriptOutput poolTxInput valRedeemer
                , mustValidateIn range
                ]

            lookup :: ScriptLookups.ScriptLookups PlutusData
            lookup = mconcat
              [ ScriptLookups.mintingPolicy listPolicy
              , ScriptLookups.validator validator
              , ScriptLookups.unspentOutputs userUtxos
              , ScriptLookups.unspentOutputs unbondedPoolUtxos
              , unbondedStateDatumLookup
              , entryDatumLookup
              ]
          pure $ constraints /\ lookup
        EQ -> do
          -- If we have equality, the on chain element must already exist, so
          -- we must spend and update it. In this case, we are updating the
          -- head of the list.
          logInfo' "userStakeUnbondedPoolContract: Compare EQ"
          assocElem <-
            liftContractM
              "userStakeUnbondedPoolContract: Cannot \
              \extract head from Assoc. List - this should be impossible"
              $ head assocList
          let
            txIn /\ txOut = snd assocElem
            mintingAction = Nothing
            valRedeemer = Redeemer $ toData $ StakeAct
              { stakeAmount: amt
              , stakeHolder: userPkh
              , mintingAction: mintingAction -- equality means we are updating the
              -- head Assoc. List element.
              }
          -- Get the Entry datum of the old assoc. list element
          logInfo_ "userStakeUnbondedPoolContract: Minting action" mintingAction
          dHash <-
            liftContractM
              "userStakeUnbondedPoolContract: Could not get Entry Datum Hash"
              $ getUtxoDatumHash txOut
          logInfo_ "userStakeUnbondedPoolContract: " dHash
          listDatum <-
            liftedM
              "userStakeUnbondedPoolContract: Cannot get Entry's\
              \ datum" $ getDatumByHash dHash
          unbondedListDatum :: UnbondedStakingDatum <-
            liftContractM
              "userStakeUnbondedPoolContract: Cannot extract NFT State datum"
              $ fromData (unwrap listDatum)
          -- The updated Entry Datum
          entryDatum <- case unbondedListDatum of
            EntryDatum { entry } -> do
              let
                e = unwrap entry
                updateDeposited = e.deposited + amtBigInt
              unless
                ( toBigInt minStake <= updateDeposited
                    && updateDeposited
                    <= toBigInt maxStake
                )
                $ throwContractError
                    "userStakeUnbondedPoolContract: Stake amount outside of \
                    \min/max range"
              pure $ Datum $ toData $ EntryDatum
                { entry: Entry $ e
                    { newDeposit = e.newDeposit + amtBigInt
                    , deposited = updateDeposited
                    }
                }
            _ -> throwContractError
              "userStakeUnbondedPoolContract: Datum not \
              \Entry constructor"

          let
            entryDatumLookup = ScriptLookups.datum entryDatum

            constraints :: TxConstraints Unit Unit
            constraints =
              mconcat
                [ mustPayToScript valHash assetDatum stakeValue
                , mustPayToScript valHash entryDatum entryValue
                , mustBeSignedBy userPkh
                , mustSpendScriptOutput txIn valRedeemer
                , mustValidateIn range
                ]

            lookup :: ScriptLookups.ScriptLookups PlutusData
            lookup = mconcat
              [ ScriptLookups.validator validator
              , ScriptLookups.unspentOutputs userUtxos
              , ScriptLookups.unspentOutputs unbondedPoolUtxos
              , entryDatumLookup
              ]
          pure $ constraints /\ lookup
        GT -> do
          -- The hashed key is greater than so we must look at the assoc. list
          -- in more detail
          logInfo' "userStakeUnbondedPoolContract: Compare GT"
          mintingAction
            /\ { firstInput, secondInput }
            /\ { firstOutput, secondOutput }
            /\ { secondKey } <-
            liftContractM
              "userStakeUnbondedPoolContract: Cannot get position in Assoc. List"
              $ findInsertUpdateElem assocList hashedUserPkh
          logInfo_ "userStakeUnbondedPoolContract: Minting action" mintingAction

          let
            valRedeemer = Redeemer $ toData $ StakeAct
              { stakeAmount: amt
              , stakeHolder: userPkh
              , mintingAction
              }

          -- Get the Entry datum of the old assoc. list (first) element
          dHash <-
            liftContractM
              "userStakeUnbondedPoolContract: Could not get Entry Datum Hash"
              $ getUtxoDatumHash firstOutput
          logInfo_ "userStakeUnbondedPoolContract: " dHash
          firstListDatum <-
            liftedM
              "userStakeUnbondedPoolContract: Cannot get \
              \Entry's datum" $ getDatumByHash dHash
          firstunBondedListDatum :: UnbondedStakingDatum <-
            liftContractM
              "userStakeUnbondedPoolContract: Cannot extract Assoc. List datum"
              $ fromData (unwrap firstListDatum)

          -- Constraints for the first element.
          firstConstraints /\ firstLookups <- case firstunBondedListDatum of
            EntryDatum { entry } -> do
              let e = unwrap entry
              firstEntryDatum <-
                if isJust mintingAction then
                  -- MintInBetween and MintEnd are the same here
                  -- a new middle entry is created so update next
                  pure $ Datum $ toData $ EntryDatum
                    { entry: Entry $ e
                        { next = Just hashedUserPkh
                        }
                    }
                else do -- depositing/updating at the first entry
                  let updateDeposited = e.deposited + amtBigInt
                  unless
                    ( toBigInt minStake <= updateDeposited
                        && updateDeposited
                        <= toBigInt maxStake
                    )
                    $ throwContractError
                        "userStakeUnbondedPoolContract: Stake amount outside of \
                        \min/max range"
                  pure $ Datum $ toData $ EntryDatum
                    { entry: Entry $ e
                        { newDeposit = e.newDeposit + amtBigInt
                        , deposited = updateDeposited
                        }
                    }
              let
                firstEntryDatumLookup = ScriptLookups.datum firstEntryDatum

                firstTxOutput :: TransactionOutput
                firstTxOutput = (unwrap firstOutput).output

                firstTxValue :: Value
                firstTxValue = (unwrap firstTxOutput).amount

                constr = mconcat
                  [ mustPayToScript valHash firstEntryDatum firstTxValue
                  , mustSpendScriptOutput firstInput valRedeemer
                  , mustPayToScript valHash assetDatum stakeValue
                  ]
                -- We add validator at the end. If we are minting i.e. when
                -- mintingAction is "Just", we include those in
                -- `middleConstraints` and `middleLookups` below
                lu = firstEntryDatumLookup
              pure $ constr /\ lu
            _ -> throwContractError
              "userStakeUnbondedPoolContract: Datum not \
              \Entry constructor"

          -- Constraints for the potential middle element.
          middleConstraints /\ middleLookups <-
            if isJust mintingAction then do -- a genuine new entry
              unless (minStake <= amt && amt <= maxStake)
                $ throwContractError
                    "userStakeUnbondedPoolContract: Stake amount outside of \
                    \min/max range"
              -- Inbetween mint - this should not fail because we have `Just`
              ma <- liftContractM
                "userStakeUnbondedPoolContract: Could not get \
                \minting action"
                mintingAction
              let
                -- Minting a new Entry
                mintRedeemer = Redeemer $ toData $ ListInsert ma

                entryDatum = Datum $ toData $ EntryDatum
                  { entry: Entry
                      { key: hashedUserPkh
                      , deposited: amtBigInt
                      , newDeposit: amtBigInt
                      , rewards: zero
                      , totalRewards: zero
                      , totalDeposited: zero
                      , open: true
                      , next: secondKey -- points to original second key
                      }
                  }

              let
                entryDatumLookup = ScriptLookups.datum entryDatum
                constr = mconcat
                  [ mustMintValueWithRedeemer mintRedeemer entryValue
                  , mustPayToScript valHash entryDatum entryValue
                  ]
                lu = mconcat
                  [ ScriptLookups.mintingPolicy listPolicy
                  , entryDatumLookup
                  ]
              pure $ constr /\ lu
            else pure $ mempty /\ mempty

          -- Get the constraints for the second assoc. list element
          lastConstraints /\ lastLookups <- case secondOutput, secondInput of
            Nothing, Nothing -> pure $ mempty /\ mempty
            Just so, Just _si -> do
              dh <-
                liftContractM
                  "userStakeUnbondedPoolContract: Could not get Entry Datum Hash"
                  $ getUtxoDatumHash so
              logInfo_ "userStakeUnbondedPoolContract: " dh
              secondListDatum <-
                liftedM
                  "userStakeUnbondedPoolContract: Cannot \
                  \get Entry's datum" $ getDatumByHash dh
              secondunBondedListDatum :: UnbondedStakingDatum <-
                liftContractM
                  "userStakeUnbondedPoolContract: Cannot extract NFT State datum"
                  $ fromData (unwrap secondListDatum)

              -- Unchanged in the case
              lastEntryDatum <- case secondunBondedListDatum of
                EntryDatum { entry } ->
                  pure $ Datum $ toData $ EntryDatum { entry }
                _ -> throwContractError
                  "userStakeUnbondedPoolContract: Datum not \
                  \Entry constructor"

              let
                lastEntryDatumLookup = ScriptLookups.datum lastEntryDatum
                constr = mempty
                lu = mconcat
                  [ lastEntryDatumLookup
                  ]
              pure $ constr /\ lu
            _, _ -> throwContractError
              "userStakeUnbondedPoolContract: Datum not\
              \Entry constructor"
          pure
            $ mconcat
                [ firstConstraints
                , middleConstraints
                , lastConstraints
                , mustBeSignedBy userPkh
                , mustValidateIn range
                ]
            /\
              mconcat
                [ ScriptLookups.validator validator
                , ScriptLookups.unspentOutputs userUtxos
                , ScriptLookups.unspentOutputs unbondedPoolUtxos
                , firstLookups
                , middleLookups
                , lastLookups
                ]
    StateDatum { maybeEntryName: _, open: false } ->
      throwContractError
        "userStakeUnbondedPoolContract: Cannot deposit to a closed pool"
    _ -> throwContractError
      "userStakeUnbondedPoolContract: \
      \Datum incorrect type"

  ubTx <- liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  pure { ubTx }
