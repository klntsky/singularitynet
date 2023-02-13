"use strict";

const BigInteger = require("big-integer");

const frontend = import("./output.js");

exports.UnbondedPool = class UnbondedPool {
  constructor(config, args, env) {
    this.config = config;
    this.args = args;
    this._contractEnv = env;
  }

  async deposit(amount, batchSize) {
    const contracts = await frontend;
    const incompleteDepositMaybe = contracts.callNothing;
    const result = await contracts.callDepositUnbondedPool(this._contractEnv)(amount)(this.args)(batchSize)(
      incompleteDepositMaybe
    )();
    return contracts.callConsumeMaybe(x => x)(x => null)(result);
  }

  async handleIncompleteDeposit(incompleteDeposit, batchSize) {
    const contracts = await frontend;
    const incompleteDepositMaybe = contracts.callJust(incompleteDeposit);
    const result = await contracts.callDepositUnbondedPool(this._contractEnv)(BigInteger(0))(this.args)(batchSize)(
      incompleteDepositMaybe
    )();
    return contracts.callConsumeMaybe(x => x)(x => null)(result);
  }

  async close(batchSize) {
    const contracts = await frontend;
    const incompleteCloseMaybe = contracts.callNothing;
    return contracts.callCloseUnbondedPool(this._contractEnv)(this.args)(batchSize)(
      incompleteCloseMaybe
    )();
  }

  async handleIncompleteClose(incompleteClose, batchSize) {
    const contracts = await frontend;
    const incompleteCloseMaybe = contracts.callJust(incompleteClose);
    const result = await contracts.callCloseUnbondedPool(this._contractEnv)(this.args)(batchSize)(
      incompleteCloseMaybe
    )();
    return contracts.callConsumeMaybe(x => x)(x => null)(result);
  }

  async userStake(amount) {
    const contracts = await frontend;
    return contracts.callUserStakeUnbondedPool(this._contractEnv)(this.args)(amount)();
  }

  async userWithdraw() {
    const contracts = await frontend;
    return contracts.callUserWithdrawUnbondedPool(this._contractEnv)(this.args)();
  }

  async adminWithdraw(addr) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callAdminWithdrawUnbondedPool(_config)(this.args)(addr)();
  }

  async getAssocList() {
    const contracts = await frontend;
    const list = await contracts.callQueryAssocListUnbondedPool(this._contractEnv)(this.args)();
    return new exports.EntryList(contracts, list);
  }
};

exports.EntryList = class EntryList {
  constructor (sdk, entries) {
    this.sdk = sdk;
    this.entries = entries.map(e => {
      return { ...e,
               rewards: Math.floor(e.rewards.value0.divide(e.rewards.value1)),
               nextCycleRewards: Math.floor(e.nextCycleRewards.value0.divide(e.nextCycleRewards.value1))
             }
    });
  }

  async byPubKeyHash(pkh) {
    for (const el of this.entries) {
      let pk = await this.sdk.callHashPkh(pkh)()
      if (arraybufferEqual(el.key.buffer, pk.buffer)) {
        return el
      }
    }
  }
}

const arraybufferEqual = (buf1, buf2) => {
  if (buf1 === buf2) {
    return true;
  }

  if (buf1.byteLength !== buf2.byteLength) {
    return false;
  }

  let view1 = new DataView(buf1);
  let view2 = new DataView(buf2);

  for (let i = 0; i < buf1.byteLength; i++) {
    if (view1.getUint8(i) !== view2.getUint8(i)) {
      return false;
    }
  }

  return true;
};

exports.createUnbondedPool = async (sdkConfig, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const contractEnv = await contracts.callMkContractEnv(config)();
  const info = await contracts.callCreateUnbondedPool(contractEnv)(initialArgs)();
  return new exports.UnbondedPool(sdkConfig, info.args, contractEnv);
};

exports.getUnbondedPool = async (sdkConfig, unbondedPoolArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const contractEnv = await contracts.callMkContractEnv(config)();
  const poolUnbondedParams = await contracts.callGetUnbondedPool(contractEnv)(unbondedPoolArgs.admin)(unbondedPoolArgs.nftCs)(unbondedPoolArgs)();
  return new exports.UnbondedPool(sdkConfig, poolUnbondedParams, contractEnv);
};

exports.getNodeTime = async (sdkConfig) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const contractEnv = await contracts.callMkContractEnv(config)();
  const time = await contracts.callGetNodeTime(contractEnv)();
  return time;
};
