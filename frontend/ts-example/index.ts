// This module demonstrates how to use the `singularitynet` TS/JS SDK to
// operate a bonded pool across the entire application lifecycle

import {
  SdkConfig,
  BondedPool,
  InitialUnbondedArgs,
  BondedPoolArgs,
  UnbondedPool,
  UnbondedPoolArgs,
} from "singularitynet";

const singularitynet = require("singularitynet");
// We need to use `big-integer` directly instead of the `BigInt` constructor
// as this package defines some methods that are required by the PS code
import BigInteger = require("big-integer");

// Runs all of the operations defined for a `BondedPool` and simulates the
// entire pool lifecycle
//
// Make sure to switch wallets as directed
const main = async () => {
  // some helpers for logging directions to switch wallets
  const admin = "ADMIN";
  const user = "USER";

  // Admin creates pool
  console.log(`STARTING AS ${admin}`);
  const nodeTime = await singularitynet.getNodeTime(localHostSdkConfig);
  console.log(nodeTime);
  const date = new Date(nodeTime);
  const delay = BigInteger(80000);
  console.log(
    `Bonded pool creation: ${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`
  );
  // Length of a staking/bonding period
  const periodLength = BigInteger(240000);

  // The initial arguments of the pool. The rest of the parameters are obtained
  // during pool creation.
  const initialBondedArgs: InitialUnbondedArgs = {
    start: nodeTime.add(delay),
    userLength: periodLength,
    bondingLength: periodLength,
    interest: { numerator: BigInteger(10), denominator: BigInteger(100) },
    minStake: BigInteger(1),
    maxStake: BigInteger(50000),
    adminLength: BigInteger(5000),
    interestLength: BigInteger(5),
    increments: BigInteger(1),
    unbondedAssetClass: {
      currencySymbol:
        "6f1a1f0c7ccf632cc9ff4b79687ed13ffe5b624cce288b364ebdce50",
      tokenName: "AGIX",
    },
  };

  const unbondedPool: UnbondedPool = await singularitynet.createUnbondedPool(
    localHostSdkConfig,
    initialBondedArgs
  );
  const unbondedPoolArgs: UnbondedPoolArgs = unbondedPool.args;
  console.log(JSON.stringify(unbondedPool))
  await logSwitchAndCountdown(user, "pool start", unbondedPoolArgs.start);

  // FIXME: Replace with code for recreating unbonded pool
  // We try recreate the pool just from its address and initial bonded args
  //const bondedPoolsArray: Array<BondedPool> = await singularitynet.getBondedPools(
  //  localHostSdkConfig,
  //  bondedPool.address,
  //  initialBondedArgs
  //);

  //// We get the first pool of the list (which should be the only one)
  //// and we use it instead of the first object.
  //const bondedPoolCopy = bondedPoolsArray[0];
  //bondedPool = bondedPoolCopy;

  // User stakes, waiting for pool start
  const userStakeAmt = BigInteger(40000);
  const r = await unbondedPool.userStake(userStakeAmt);
  console.log(JSON.stringify(r))
  await logSwitchAndCountdown(
    admin,
    "bonding period",
    unbondedPoolArgs.start.add(unbondedPoolArgs.userLength)
  );

  // Admin deposits to pool
  const depositBatchSize = BigInteger(1);
  await unbondedPool.deposit(depositBatchSize, []);
  await logSwitchAndCountdown(
    user,
    "withdrawing  period",
    unbondedPoolArgs.start.add(
      unbondedPoolArgs.userLength).add(
      unbondedPoolArgs.bondingLength)
  );

  // User withdraws
  await unbondedPool.userWithdraw();
  //await logSwitchAndCountdown(admin, "closing period";

  // Admin closes pool
  const closeBatchSize = BigInteger(10);
  await unbondedPool.close(closeBatchSize, []);

  console.log("Pool closed");
};

const localHostSdkConfig: SdkConfig = {
  ctlServerConfig: {
    host: "35.175.138.251",
    port: 8081,
    secure: false,
    path: "",
  },
  ogmiosConfig: {
    host: "35.175.138.251",
    port: 1337,
    secure: false,
    path: "",
  },
  datumCacheConfig: {
    host: "35.175.138.251",
    port: 9999,
    secure: false,
    path: "",
  },
  networkId: 0,
  logLevel: "Info",
  walletSpec: "Eternl",
};

// Helpers

const logSwitchAndCountdown = async (
  who: "USER" | "ADMIN", // the wallet to switch to
  what: string, // what we're waiting for
  time: BigInteger // how long to wait
) => {
  console.log(`SWITCH WALLETS NOW - CHANGE TO ${who}`);
  const _input = prompt("Press OK after switching.");
  console.log(`Waiting for ${what}...`);
  await countdownTo(Number(time));
};

const countdownTo = async (tf: number) => {
  let now = await singularitynet.getNodeTime(localHostSdkConfig);
  while (now <= tf) {
    console.log(`${now} <= ${tf}`);
    console.log(`Countdown: ${showSecondsDiff(tf, now)}`);
    await sleep(10000);
    now = await singularitynet.getNodeTime(localHostSdkConfig);
  }
  console.log(`${now} > ${tf}`);
  console.log(`0`);
};

const sleep = async (ms: number) => new Promise((r) => setTimeout(r, ms));

const showSecondsDiff = (x: number, y: number) => (x - y) / 1000;

// Run the contracts, see `main` above
main()
  .then((_) => {})
  .catch((e) => console.log(e));
