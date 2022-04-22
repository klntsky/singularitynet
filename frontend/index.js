"use strict";

// This needs to be asynchronous to load the WASM from CSL
import("./exe/Main.purs").then((m) => m.main());

if (module.hot) {
  module.hot.accept();
}

console.log("app starting");
