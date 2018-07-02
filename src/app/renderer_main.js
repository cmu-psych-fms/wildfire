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

// Be able to update the data dir and have it take immediate effect

// change asteroids to spheres to that bounce and chase the player if
// they get too close. They bounce off the start locations.  maybe
// they appear when a fortress is destroyed and have a lifespan before
// disappearing below the surface.

// maybe both players have to hit the sphere to make it disappear.

// when you hit a sphere it chases the player who hit it. but you have
// to wait for the other player to shoot them to get rid of them.

// they are all the same size. fortresses release a configurable
// number of them upon destruction.

// draw a line between ball and target player. if a wall intersects
// the line then the ball bounces deterministally. maybe it also loses
// its target. in order to destroy the ball it must have a
// target. when it has a target its color changes maybe to red.

// 50 points to destroy the squere. but they only last maybe 30
// seconds.

// player missiles destroys other players

// must do something about camera angle to reduce chances of nausea.

// maybe a camera mode where the camera is always pointing in the same
// direction but it's position moves in order to keep the ship at the
// center. like the ultima 8 camera angle. the "isometric" camera.

// maybe player can customize distance with +/- keys.
