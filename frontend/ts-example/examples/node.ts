/*

 This module demonstrates how to utilise external files with payment
 and staking credentials to execute transactions using the SDK.

 It is important to note this will only work in a NodeJS environment and not
 in the browser.

 ### How to create a key-pair

 Here are the steps to create a payment (and optionally staking) key pair with
 `cardano-cli`:

 1. Enter a nix shell with `cardano-cli`:
 
    `nix shell "github:input-output-hk/cardano-node"#cardano-cli`
 
 2. Create a payment key:
 
    `cardano-cli address key-gen --normal-key --verification-key-file FILE --signing-key-file FILE`
 
 3. (OPTIONAL) Create a staking key:
 
    `cardano-cli stake-address key-gen --verification-key-file FILE --signing-key-file FILE`
 
 4. (OPTIONAL) Build a Shelley payment address. This is useful for checking the
    current funds and making deposits to it. For this you need the
    *verification* files created in steps 2 and 3.
 
    `cardano-cli address build --payment-verification-key-file FILE --staking-verification-key-file FILE (--mainnet | --testnet-magic N)`
*/

import {
  InitialUnbondedArgs,
  UnbondedPool,
  getNodeTime,
  createUnbondedPool
} from "singularitynet";

// We need to use `big-integer` directly instead of the `BigInt` constructor
// as this package defines some methods that are required by the PS code
import BigInteger = require("big-integer");

import { mlabsSdkConfig } from "./utils";

export const main = async () => {
  const admin = "ADMIN";
  // We use the KeyWallet support of the SDK
  mlabsSdkConfig.walletSpec = { 
    privatePaymentKeyPath: "payment.skey",
    privateStakingKeyPath: "staking.skey"
  };

  // Admin creates pool
  console.log(`STARTING AS ${admin}`);
  const nodeTime = await getNodeTime(mlabsSdkConfig);
  const date = new Date(nodeTime.toJSNumber());
  const delay = BigInteger(10000);
  console.log(
    `Bonded pool creation: ${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`
  );

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

  let unbondedPool: UnbondedPool = await createUnbondedPool(
    mlabsSdkConfig,
    initialUnbondedArgs
  );
  console.log("Pool arguments", JSON.stringify(unbondedPool.args))
};
