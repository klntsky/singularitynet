import {
  InitialUnbondedArgs,
  UnbondedPool,
  SdkConfig,
  getNodeTime,
  createUnbondedPool,
} from "singularitynet";

// We need to use `big-integer` directly instead of the `BigInt` constructor
// as this package defines some methods that are required by the PS code
import bigInt from "big-integer";
import yargs from "yargs/yargs";
import { hideBin } from "yargs/helpers";


// We create the CLI and parse the arguments
const args = yargs(hideBin(process.argv)).options({
  adminPaymentKeyPath: {
     type: 'string',
     default: 'keys/admin-payment.skey',
     describe: "The path to the admin's private payment key" },
  adminStakingKeyPath: {
     type: 'string',
     default: 'keys/admin-staking.skey',
     describe: "The path to the admin's private staking key" },
  poolStart: {
    type: 'string',
    describe: "Pool's starting time as a POSIX timestamp in _milliseconds_"
  },
  userLength: {
    type: 'number',
    default: 15,
    describe: "User period's length in _minutes_"
  },
  adminLength: {
    type: 'number',
    default: 15,
    describe: "Admin period's length in _minutes_"
  },
  depositAmount: {
    type: 'number',
    default: 50_000,
    describe: "The amount of AGIX to deposit each cycle"
  },
  cyclesCount : {
    type: 'number',
    default: 3,
    describe: "Number of cycles to execute before closing the pool"
  },
  ogmiosUrl : {
    type: 'string',
    default: 'ogmios.preprod.ctl-runtime.staging.mlabs.city',
    describe: "URL for Ogmios' service. By default it uses MLabs public instance"
  },
  kupoUrl : {
    type: 'string',
    default: 'kupo.preprod.ctl-runtime.staging.mlabs.city',
    describe: "URL for kupo's service. By default it uses MLabs public instance"
  },
  ogmiosDatumCacheUrl : {
    type: 'string',
    default: 'ogmios-datum-cache.preprod.ctl-runtime.staging.mlabs.city',
    describe: "URL for ODC's service. By default it uses MLabs public instance"
  },
}).parseSync();

// We create the SDK Config
const sdkConfig: SdkConfig = {
  ogmiosConfig: {
    host: args.ogmiosUrl,
    port: 443,
    secure: true,
    path: "",
  },
  kupoConfig: {
    host: args.kupoUrl,
    port: 443,
    secure: true,
    path: "",
  },
  datumCacheConfig: {
    host: args.ogmiosDatumCacheUrl,
    port: 443,
    secure: true,
    path: "",
  },
  networkId: 0,
  logLevel: "Info",
  walletSpec: {
    privatePaymentKeyPath: args.adminPaymentKeyPath
    , privateStakingKeyPath: args.adminStakingKeyPath
  },
};

// We initialise the pool based on the parsed arguments
console.log("Creating the pool...")
const poolStart =
  args.poolStart == undefined ?
    await getNodeTime(sdkConfig) :
    bigInt(args.poolStart);
const userLength = bigInt(args.userLength * 60 * 1000)
const adminLength = bigInt(args.adminLength * 60 * 1000)

const initialUnbondedArgs: InitialUnbondedArgs =  {
  start: poolStart,
  userLength: userLength,
  bondingLength: bigInt(0),
  interestLength: bigInt(0),
  adminLength: adminLength,
  interest: { numerator: bigInt(10), denominator: bigInt(100) },
  minStake: bigInt(100000000),
  maxStake: bigInt(5000000000000),
  increments: bigInt(1),
  unbondedAssetClass: {
    currencySymbol:
      "6f1a1f0c7ccf632cc9ff4b79687ed13ffe5b624cce288b364ebdce50",
    tokenName: "AGIX",
  }
}

const startDate = new Date(poolStart.toJSNumber());
console.log(
  `Bonded pool creation: ${startDate.getHours()}:${startDate.getMinutes()}:${startDate.getSeconds()}`
);

const unbondedPool: UnbondedPool = await createUnbondedPool(
  sdkConfig,
  initialUnbondedArgs
);
console.log("Pool arguments", JSON.stringify(unbondedPool.args))

// We create the stopping times for all the deposits and the closing of the pool
const depositTimes : number[] = [...Array(args.cyclesCount - 1)].map((_, i) =>
  poolStart.toJSNumber() + userLength.toJSNumber() + i * (userLength.toJSNumber() + adminLength.toJSNumber()))

console.log(depositTimes)

const closeTime : number = poolStart.toJSNumber() + userLength.toJSNumber() + (args.cyclesCount - 1) * (userLength.toJSNumber() + adminLength.toJSNumber())

console.log(closeTime)

/** We write a function for waiting until a certain time. To avoid polling the
 *  node constantly, we wait until the local time is equal to the stop time.
 *
 *  Only then we poll the node to make sure the on-chain time is at least the
 *  stop time.
 */
const countdownTo = async (tf: number) => {
  console.log("Waiting until ", new Date(tf).toISOString(), "...");
  const localNow = Date.now();
  await sleep(tf - localNow)
  console.log("Done.")

  console.log("Waiting for Node to catch up...")
  let now = await getNodeTime(sdkConfig);
  let nowNum = now.toJSNumber();
  while (nowNum <= tf) {
    console.log("...");
    await sleep(20_000);
    now = await getNodeTime(sdkConfig);
    nowNum = now.toJSNumber();
  }
  console.log("Done.");
};

/** Sleep a give number of milliseconds */
const sleep = async (ms: number) => new Promise((r) => setTimeout(r, ms));

// We start the waiting-deposit loop.
for (const stopTime of depositTimes) {
  console.log("Waiting to perform next deposit...");
  await countdownTo(stopTime);
  unbondedPool.deposit(bigInt(args.depositAmount), bigInt(0))
};

// Now we wait for pool closing
console.log("Waiting to close the pool");
await countdownTo(closeTime)
const closeResult = await unbondedPool.close(bigInt(0));

if (closeResult == null) {
  console.log("Failed to close the pool");
  process.exit(1);
} else {
  console.log("Pool closed succesfully");
}

process.exit(0);
