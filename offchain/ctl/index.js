"use strict";

// This needs to be asynchronous to load the WASM from CSL
import("./SingularityNet/Test.purs").then((m) => m.main());

if (module.hot) {
  module.hot.accept();
}

console.log("app starting");
