import {
    main
  } from './examples/default';

// Run the contracts, see `main` above.
//
// NOTE: If this is run from NodeJS, it is important to explicitly finish the
// process, since otherwise the process will continue to run after an error is
// thrown or the program finishes succesfully. This is related to the SDK not
// properly releasing some resources, which ends up blocking the Node process
// from exiting.
main()
  .then((_) => {
    //process.exit();
    })
  .catch((e) => {
    console.log(e);
    //process.exit();
    }) ;
