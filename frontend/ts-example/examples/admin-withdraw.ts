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

  // User 1 stakes
  await logSwitchAndCountdown(user1, "cycle #0, user period #0", unbondedPoolArgs.start);
  const userStakeAmt1 = BigInteger(40000);
  const r0 = await unbondedPool.userStake(userStakeAmt1);
  console.log(JSON.stringify(r0))
  const l0 = await unbondedPool.getAssocList()
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
  const user_addr = prompt("Please enter user's address here: ");
  const r1 = await unbondedPool.adminWithdraw(user_addr);
  console.log(JSON.stringify(r1));
  const l1 = await unbondedPool.getAssocList()
  console.log(JSON.stringify(l1))

};
