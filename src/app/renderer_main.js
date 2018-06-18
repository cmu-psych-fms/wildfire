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
for (let i=0; i<interfaces.length; i++) {
    sl.innerHTML += '<p>Server running at http://'+interfaces[i][1]+':3000';
}

var server = new es.ExperimentServer(settings.get('datadir'), path.join(__dirname, 'browser'));
