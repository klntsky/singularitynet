"use strict";

const BigInteger = require("big-integer");

const frontend = import("./output.js");

exports.BondedPool = class BondedPool {
  constructor(config, args, address) {
    this.config = config;
    this.args = args;
    this.address = address;
    this._config = this._getConfig(config);
  }

  async _getConfig(config) {
    const contracts = await frontend;
    const _config = await contracts.buildContractConfig(config)();
    this._config = _config;
  }

  async deposit(amount, batchSize, idxArray) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callDepositBondedPool(_config)(this.args)(amount)(
      idxArray
    )();
  }

  async close(batchSize, idxArray) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callCloseBondedPool(_config)(this.args)(batchSize)(
      idxArray
    )();
  }

  async userStake(amount) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callUserStakeBondedPool(_config)(this.args)(amount)();
  }

  async userWithdraw() {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callUserWithdrawBondedPool(_config)(this.args)();
  }
};

exports.UnbondedPool = class UnbondedPool {
  constructor(config, args, address, env) {
    this.config = config;
    this.args = args;
    this.address = address;
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

exports.createBondedPool = async (sdkConfig, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const contractEnv = await contracts.callMkContractEnv(config)();
  const info = await contracts.callCreateBondedPool(contractEnv)(initialArgs)();
  return new exports.BondedPool(sdkConfig, info.args, info.address, contractEnv);
};

exports.getBondedPools = async (sdkConfig, address, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const contractEnv = await contracts.callMkContractEnv(config)();
  const poolsBondedParams = await contracts.callGetBondedPools(contractEnv)(address)(initialArgs)();
  let pools = [];
  for (const bondedParams of poolsBondedParams) {
      pools.push(new exports.BondedPool(sdkConfig, bondedParams, address, contractEnv));
  }
  return pools;
};

exports.createUnbondedPool = async (sdkConfig, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const contractEnv = await contracts.callMkContractEnv(config)();
  const info = await contracts.callCreateUnbondedPool(contractEnv)(initialArgs)();
  return new exports.UnbondedPool(sdkConfig, info.args, info.address, contractEnv);
};

exports.getUnbondedPools = async (sdkConfig, address, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const contractEnv = await contracts.callMkContractEnv(config)();
  const poolsUnbondedParams = await contracts.callGetUnbondedPools(contractEnv)(address)(initialArgs)();
  let pools = [];
  for (const unbondedParams of poolsUnbondedParams) {
      pools.push(new exports.UnbondedPool(sdkConfig, unbondedParams, address, contractEnv));
  }
  return pools;
};

exports.getNodeTime = async (sdkConfig) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const contractEnv = await contracts.callMkContractEnv(config)();
  const time = await contracts.callGetNodeTime(contractEnv)();
  return time;
};
