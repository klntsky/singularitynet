import { SdkConfig } from "singularitynet";
const singularitynet = require("singularitynet");
import BigInteger = require("big-integer");

export const mlabsSdkConfig: SdkConfig = {
  ogmiosConfig: {
    host: "ogmios.preprod.ctl-runtime.staging.mlabs.city",
    port: 443,
    secure: true,
    path: "",
  },
  kupoConfig: {
    host: "kupo.preprod.ctl-runtime.staging.mlabs.city",
    port: 443,
    secure: true,
    path: "",
  },
  datumCacheConfig: {
    host: "ogmios-datum-cache.preprod.ctl-runtime.staging.mlabs.city",
    port: 443,
    secure: true,
    path: "",
  },
  networkId: 0,
  logLevel: "Info",
  walletSpec: "Eternl",
};

// Helpers

export const logSwitchAndCountdown = async (
  who: "USER1" | "USER2" | "ADMIN", // the wallet to switch to
  what: string, // what we're waiting for
  time: BigInteger.BigInteger // how long to wait
) => {
  console.log(`SWITCH WALLETS NOW - CHANGE TO ${who}`);
  prompt("Press OK after switching.");
  console.log(`Waiting for ${what}...`);
  await countdownTo(Number(time));
};

export const countdownTo = async (tf: number) => {
  let now = await singularitynet.getNodeTime(mlabsSdkConfig);
  while (now <= tf) {
    console.log(`Countdown: ${showSecondsDiff(tf, now)}s`);
    await sleep(20_000);
    now = await singularitynet.getNodeTime(mlabsSdkConfig);
  }
  console.log(`Countdown over`);
};

export const sleep = async (ms: number) => new Promise((r) => setTimeout(r, ms));

export const showSecondsDiff = (x: number, y: number) => (x - y) / 1000;
