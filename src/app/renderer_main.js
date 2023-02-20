/**
 * @license
 * Copyright 2018,2019 Shawn Betts
 * SPDX-License-Identifier: MIT
**/

// Specifying wsEngine as an option to socketio doesn't seem to make
// it use ws (and not uws). So tell it through the env var.
process.env.EIO_WS_ENGINE = 'ws';

var app = require('electron').remote.app;
var dialog = require('electron').remote.dialog;
var settings = require('electron-settings');
var path = require('path');
var fs = require('fs');
var os = require('os');

var es = require('./experiment_server');
var ms = require('./ModelServer');


if (!settings.has('datadir')) {
    settings.set('datadir', path.join(app.getPath('userData'), 'geospatial-data').toString());
}

function getNetworkInterfaces () {
    var ret = [];
    var ifaces = os.networkInterfaces();
    Object.keys(ifaces).forEach(function (ifname) {
        var alias = 0;

        ifaces[ifname].forEach(function (iface) {
            if ('IPv4' !== iface.family || iface.internal !== false) {
                // skip over internal (i.e. 127.0.0.1) and non-ipv4 addresses
                return;
            }

            if (alias >= 1) {
                // this single interface has multiple ipv4 addresses
                ret.push([ifname + ':' + alias, iface.address]);
            } else {
                // this interface has only one ipv4 adress
                ret.push([ifname, iface.address]);
            }
            ++alias;
        });
    });
    return ret;
}

function setupUserSettings() {
    var dd = document.getElementById('data-dir');
    dd.innerHTML = settings.get('datadir') + '/';
    document.getElementById('change').addEventListener('click', function () {
        dialog.showOpenDialog({properties: ['openDirectory', 'createDirectory']},
                              function (things) {
                                  settings.set('datadir', things[0]);
                                  dd.innerHTML = things[0] + '/';
                              });
    });
}

setupUserSettings();

try { fs.mkdirSync(settings.get('datadir')); }
catch (e) { if (e.code !== 'EEXIST') throw e; }

console.log(path.join(__dirname, 'browser'));

var sl = document.getElementById('server_logging');
var interfaces = getNetworkInterfaces();

var baselog = function (lib, args) {
    var line = '';
    for (let i=0; i<args.length; i++) {
        line += args[i];
        line += ' ';
    }
    sl.innerHTML += '<br>'+lib+': ' + line;
}

var eslog = function () {
    baselog('GameServer', arguments);
}

for (let i=0; i<interfaces.length; i++) {
    eslog('Server running at http://'+interfaces[i][1]+':3100');
}

var server = new es.ExperimentServer(settings.get('datadir'),
                                     path.join(__dirname, 'browser'),
                                     eslog);

var lf = '\r\n';
var port = 3000;

// var mslog = console.log;

var mslog = function () {
    baselog('ModelServer', arguments);
}

function ModelInterface() {
    ms.ModelInterface.apply(this, arguments);
}

ModelInterface.prototype = Object.create(ms.ModelInterface.prototype);

ModelInterface.prototype.logMsg = function (dest, msg) {
    mslog(dest + ':' + msg);
}

var mserver = ms.startServer(mslog,
                             function (sock, id) {
                                 return new ModelInterface(sock, id,
                                                           {lineEnding: lf});
                             },
                             {port: port});
