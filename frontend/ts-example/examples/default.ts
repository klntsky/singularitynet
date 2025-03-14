// This module demonstrates how to use the `singularitynet` TS/JS SDK to
// operate a bonded pool across the entire application lifecycle
import {
  InitialUnbondedArgs,
  UnbondedPool,
  UnbondedPoolArgs,
} from "singularitynet";

const singularitynet = require("singularitynet");
// We need to use `big-integer` directly instead of the `BigInt` constructor
// as this package defines some methods that are required by the PS code
import BigInteger = require("big-integer");

import { logSwitchAndCountdown, mlabsSdkConfig } from "./utils";

// Runs all of the operations defined for a `BondedPool` and simulates the
// entire pool lifecycle
// Make sure to switch wallets as directed
export const main = async () => {
  // some helpers for logging directions to switch wallets
  const admin = "ADMIN";
  const user1 = "USER1";
  const user2 = "USER2";

  // Admin creates pool
  console.log(`STARTING AS ${admin}`);
  const nodeTime = await singularitynet.getNodeTime(mlabsSdkConfig);
  console.log(nodeTime);
  const date = new Date(nodeTime);
  const delay = BigInteger(10000);
  console.log(
    `Bonded pool creation: ${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`
  );

  const cycleLength = BigInteger(240000 + 1000 + 180000);

  // The initial arguments of the pool. The rest of the parameters are obtained
  // during pool creation.
  const initialUnbondedArgs: InitialUnbondedArgs = {
    start: nodeTime.add(delay),
    userLength: BigInteger(300000),
    bondingLength: BigInteger(1000),
    interestLength: BigInteger(1000),
    adminLength: BigInteger(180000),
    interest: { numerator: BigInteger(10), denominator: BigInteger(100) },
    minStake: BigInteger(1),
    maxStake: BigInteger(50000),
    increments: BigInteger(1),
    unbondedAssetClass: {
      currencySymbol:
        "6f1a1f0c7ccf632cc9ff4b79687ed13ffe5b624cce288b364ebdce50",
      tokenName: "AGIX",
    },
  };

  let unbondedPool: UnbondedPool = await singularitynet.createUnbondedPool(
    mlabsSdkConfig,
    initialUnbondedArgs
  );
  const unbondedPoolArgs: UnbondedPoolArgs = unbondedPool.args;
  console.log(JSON.stringify(unbondedPool))

  // We try to recreate the pool from the admin's PKH, the state CS and the initial bonded args.
  // This is just for testing pool reconstruction.
  const unbondedPoolCopy = await singularitynet.getUnbondedPool(
    mlabsSdkConfig,
    unbondedPoolArgs
  );
  // We use the copy, it shouldn't make a difference
  unbondedPool = unbondedPoolCopy;

  // User 1 stakes
  await logSwitchAndCountdown(user1, "cycle #0, user period #0", unbondedPoolArgs.start);
  const userStakeAmt1 = BigInteger(40000);
  const r0 = await unbondedPool.userStake(userStakeAmt1);
  console.log(JSON.stringify(r0))
  const l0 = await unbondedPool.getAssocList()
  console.log(JSON.stringify(l0))

  // User 2 stakes
  await logSwitchAndCountdown(user2, "cycle #0, user period #0", unbondedPoolArgs.start);
  const userStakeAmt2 = BigInteger(20000);
  const r1 = await unbondedPool.userStake(userStakeAmt2);
  console.log(JSON.stringify(r0))
  const l1 = await unbondedPool.getAssocList()
  console.log(JSON.stringify(l0))

  // Admin deposits to pool
  // The admin does not deposit anything (since no promise has been made yet),
  // but promises to deposit 40_000 AGIX to the users currently present in the
  // pool if they keep their stakes until the next cycle's admin period.
  await logSwitchAndCountdown(
    admin,
    "cycle #0, admin period #1",
    unbondedPoolArgs.start.add(unbondedPoolArgs.userLength)
  );
  const depositBatchSize = BigInteger(0);
  const adminDeposit = BigInteger(40000);
  const r2 = await unbondedPool.deposit(adminDeposit, depositBatchSize);
  console.log(JSON.stringify(r2));
  const l2 = await unbondedPool.getAssocList()
  console.log(JSON.stringify(l2))

  // User 1 withdraws during user period
  // Since the user did not stay for a full cycle, they will get zero rewards.
  await logSwitchAndCountdown(
    user1,
    "cycle #1, user period #3",
    unbondedPoolArgs.start.add(
      unbondedPoolArgs.userLength).add(
      unbondedPoolArgs.adminLength).add(
      unbondedPoolArgs.bondingLength));
  const r3 = await unbondedPool.userWithdraw();
  console.log(JSON.stringify(r3));
  const l3 = await unbondedPool.getAssocList()
  console.log(JSON.stringify(l3))

  // User 2 stakes during user period
  // This new stake will only count for the reward *after* the next one.
  // However, we will soon see that there will be no more rewards after
  // the next one because the admin will close the pool.
  await logSwitchAndCountdown(
    user2,
    "cycle #1, user period #3",
    unbondedPoolArgs.start.add(
      unbondedPoolArgs.userLength).add(
      unbondedPoolArgs.adminLength).add(
      unbondedPoolArgs.bondingLength)
  );
  const r4 = await unbondedPool.userStake(userStakeAmt2);
  console.log(JSON.stringify(r4));

  // Admin closes pool
  await logSwitchAndCountdown(
      admin,
      "admin period",
      unbondedPoolArgs.start.add(
        unbondedPoolArgs.userLength).add(
        unbondedPoolArgs.adminLength).add(
        unbondedPoolArgs.bondingLength).add(
        unbondedPoolArgs.userLength));

  const closeBatchSize = BigInteger(10);
  const r5 = await unbondedPool.close(closeBatchSize);
  console.log(JSON.stringify(r5));

  // User 2 withdraws their rewards and stake after pool closure.
  await logSwitchAndCountdown(
      user2,
      "admin period",
      unbondedPoolArgs.start.add(
        unbondedPoolArgs.userLength).add(
        unbondedPoolArgs.adminLength).add(
        unbondedPoolArgs.bondingLength).add(
        unbondedPoolArgs.userLength).add(
        unbondedPoolArgs.adminLength));

  const r6 = await unbondedPool.userWithdraw();
  console.log(JSON.stringify(r6));

};
