/**
 * @license
 * Copyright 2018,2019 Shawn Betts
 * SPDX-License-Identifier: MIT
**/

const args = require('yargs')
      .option('data', {
          describe: "Specify the location of data files",
          default: "data"})
      .option('client', {
          describe: "Specify the location of client files",
          default: "../browser"})
      .help()
      .argv;

var es = require('./experiment_server');

var server = new es.ExperimentServer(args.data, args.client);
