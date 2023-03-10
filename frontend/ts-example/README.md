# `singularitynet` Typescript example

This subdirectory contains a new Typescript project demonstrating how to consume the [Javascript SDK](../package.json) that we created. This document outlines the necessary steps to build and bundle the main JS SDK and then build the Typescript project. It also details some important details for your own frontend integration.

**Table of Contents**

- [How to build and run](#how-to-build-and-run)
  - [Build the main SDK package](#build-the-main-sdk-package)
  - [Build the Typescript project](#build-the-typescript-project)
- [Other important notes](#other-important-notes)
  - [Type definitions](#type-definitions)
  - [Creating the pools](#creating-the-pools)
    - [Changing the service configurations](#changing-the-service-configurations)
  - [Using `big-integer` instead of native `bigint`s](#using-big-integer-instead-of-native-bigints)
  - [The `BROWSER_RUNTIME` environment variable](#the-browser_runtime-environment-variable)
  - [Webpack](#webpack)

## How to build and run

### Build the main SDK package

We will simulate pulling the main SDK package from NPM by using the SDK directory (i.e. `frontend`) as a local NPM dependency.

_`frontend/ts-example/package.json`_:

```json-with-comments
{
  "name": "singularitynet-example",
  // Omitted for brevity
  "dependencies": {
    "singularitynet": "file:../"
  }
}
```

Since we haven't published the package, we need to build it first using the following steps:

1. Enter the root of the repository
2. Enter the Nix development environment and then the `frontend` directory:
   ```
   $ nix develop .#frontend
   $ cd frontend
   ```
3. Compile and bundle the Purescript code.
   ```
   $ npm run bundle
   ```

This will compile all the Purescript source files under `src/` and produce an
`output.js` file, which implements most of the functionality made available
under `index.d.ts`. *Only* after this step is done, the library is complete and
available to be used as a dependency in other packages.

The library is called `singularitynet` and with this name it can be imported
by package consumers (see the examples' [Webpack configuration](webpack.config.js).

### Build the Typescript project (for the browser)

We're now ready to consume the (local) SDK package. If you make any changes in
the `frontend` directory, you can easily repeat the previous step to re-build
it and update it as a dependency for the TS project.

There are a few examples under `ts-example/examples/` which can be loaded by
changing which one is imported in `index.ts`:

_frontend/ts-example/index.ts_

```
import {
    main
  } from './examples/default'; // Change which example to run here.
```

These are the instructions for building the TS project for the browser:

1. Enter the `frontend/ts-example` directory
2. Install the NPM dependencies to the local directory, including the `singularitynet` package
   ```
   $ npm i
   ```
   **Note**: We do **not** do this before building the main SDK package as the NPM dependencies are handled by Nix in that case. For the current TS example, however, I did not create a similar setup so we need to use `npm` directly.
3. If you're running the CTL runtime on `localhost`, make sure that's started. Otherwise, change the configurations in `index.ts` (see [below](#changing-the-service-configurations))
4. Either start the development server or run the build:
   ```
   $ npm run web:dev
   ```
   And visit `localhost:4008` in your browser, _or_
   ```
   $ npm run web:build

   ```
### Build the Typescript project (for NodeJS)

To run the Typescript project in a NodeJS environment, make follow the same steps as before, but in step 4
instead run the following:

```
$ npm run node:build
```

And then run the resulting executable under `ts-built`:

```
$ node ts-built/index.js
```

But before doing this, *make sure to run an example that works under NodeJS*. For now, the only example
made to run in a Node environment is `example/node.ts`.

## Other important notes

This section outlines some things you should be aware of when consuming the main JS SDK. There are examples of each in this TS example. When writing your own JS/TS modules, please take all of these considerations into account.

### Type definitions

The `default` example makes use of several type definitions, but there are several more defined for the SDK.
Have a look at the TS [declarations](../index.d.ts) for the main SDK to see all of the defined types.
All of these can be `import`ed normally in your own integrations, e.g.

```typescript
import {
  SdkConfig,
  LogLevel,
  SdkServerConfig,
  UnbondedPool,
} from "singularitynet";
```

### Creating the pools

`index.d.ts` defines one immutable class that represents unbonded pools (`UnbondedPool`).
**Do not** use the constructors directly. Instead, use the asynchronous `createUnbondedPool` function.
This is because the initializations of each pool must be done asynchronously and JS does not allow `async` constructors.
For example

```typescript
const main = async () => {
  const pool: UnbondedPool = await createBondedPool(
    someSdkConfig,
    someInitialArgs
  );
  await pool.deposit(BigInteger(1), BigInteger(0));
  // more pool operations...
};
```

#### Changing the service configurations

The `UnbondedPool` takes an `SdkConfig` as their first argument which includes
information for the contracts to connect to the required runtime services (`ogmios`, `kupo`, etc...).
For the purposes of this TS example, we are using MLabs hosted infrastructure for all services:

_frontend/ts-example/examples/utils.ts_
```typescript
import { SdkConfig } from "singularitynet";

// omitted for brevity

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
```

### Using `big-integer` instead of native `bigint`s

Because we use a Purescript package that depends on `big-integer`, an NPM package which extends the `bigint` type with additional methods, you **cannot** use `bigint`s or the `BigInt` constructor directly. The correct approach is illustrated by the following:

```typescript
import { BigInteger } from "big-integer";
import { InitialBondedArgs } from "singularitynet";

const BigInteger = require("big-integer");

const initialBondedArgs: InitialBondedArgs = {
  iterations: BigInteger(2), // NOT `BigInt`
  // omitted for brevity
};

const someFn = (
  x: BigInteger // NOT `bigint`
) => {};
```

### The `BROWSER_RUNTIME` environment variable

CTL depends directly on `cardano-serialization-lib` which publishes several incompatible packages for different environments (the browser, NodeJS, ASM.js, etc...). It isn't possible to polyfill these, so the CTL authors have introduced a `BROWSER_RUNTIME` environment variable to determine the environment and load the correct package.

When bundling for the web, it is important to select the `-browser` package, so we must set `BROWSER_RUNTIME=1` when building (that is, when invoking Webpack). This is illustrated by the following:

_`frontend/ts-example/package.json`_:

```json-with-comments
{
  "name": "singularitynet-example",
  "scripts": {
    "web:dev": "BROWSER_RUNTIME=1 webpack-dev-server --mode=development",
    "web:build": "BROWSER_RUNTIME=1 webpack --mode=production",
    // Omitted for brevity
  },
  // Omitted for brevity
}
```

It is also necessary to add this as a plugin in our Webpack configurations so it is always passed through, for example

_`frontend/ts-example/webpack.config.js`_:

```javascript
const webpack = require("webpack");

module.exports = {
  // omitted for brevity
  plugins: [
    new webpack.DefinePlugin({
      BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME,
    }),
    // other plugins here
  ];
};
```

**Make sure** to do the same when writing your own integrations!

### Webpack

For greatest comptability, we recommend using the same version of Webpack (`v5.67.0`) as we do, with the same `experiments` enabled. This can be fragile due to our somewhat unique circumstances (our dependencies needing to load WASM using top-level `await`). The relevant `experiments` include

_`frontend/ts-example/webpack.config.js`_:

```javascript
module.exports = {
  experiments: {
    asyncWebAssembly: false,
    layers: false,
    lazyCompilation: false,
    outputModule: true,
    syncWebAssembly: true,
    topLevelAwait: true,
  },
  // omitted for brevity
};
```
