var engine = require('./gameengine');
var config = require('./config');

var fs = require('fs');
var PNG = require('pngjs').PNG;

function GameServer() {
    this.numPlayers = 0;
    this.players = {};
    this.engine = new engine.GameEngine(new config.Config());
    // this.engine.placeFortresses(10, 50);
    // this.engine.placeAsteroids(10, 50);
}

GameServer.prototype = {};

GameServer.prototype.readMap = function (file) {
    this.mapFileName = file;
    var data = fs.readFileSync(file);
    var png = PNG.sync.read(data);

    this.engine.map.width = png.width;
    this.engine.map.height = png.height;
    this.engine.map.data = new Array(png.width * png.height);
    this.engine.map.viewPort = {x: 100, y:100, w:100, h:100};

    for (let x=0; x<png.width; x++) {
        for (let y=0; y<png.height; y++) {
            var m = y*png.height+x;
            var idx = m << 2;
            if (png.data[idx] === 0 && png.data[idx+1] === 255 && png.data[idx+2] === 0) {
                this.engine.map.data[m] = engine.MAP_TREE;
            } else if (png.data[idx] === 0 && png.data[idx+1] === 0 && png.data[idx+2] === 0) {
                this.engine.map.data[m] = engine.MAP_GRASS;
            } else if (png.data[idx] === 255 && png.data[idx+1] === 255 && png.data[idx+2] === 0) {
                this.engine.map.data[m] = engine.MAP_HOUSE;
            } else if (png.data[idx] === 100 && png.data[idx+1] === 100 && png.data[idx+2] === 100) {
                this.engine.map.data[m] = engine.MAP_HROAD;
            } else if (png.data[idx] === 101 && png.data[idx+1] === 101 && png.data[idx+2] === 101) {
                this.engine.map.data[m] = engine.MAP_VROAD;
            } else if (png.data[idx] === 0 && png.data[idx+1] === 0 && png.data[idx+2] === 255) {
                this.engine.map.data[m] = engine.MAP_WATER;
            } else if (png.data[idx] === 255 && png.data[idx+1] === 255 && png.data[idx+2] === 255) {
                this.engine.map.data[m] = engine.MAP_ROCK;
            } else if (png.data[idx] === 255 && png.data[idx+1] === 255 && png.data[idx+2] === 0) {
                this.engine.map.data[m] = engine.MAP_HOUSE;
            } else {
                this.engine.map.data[m] = engine.MAP_GRASS;
            }
        }
    }
    this.engine.startSomeFires();
}

GameServer.prototype.getConnectPayload = function (client) {
    var payload = { id: client.userid,
                    map: this.engine.map,
                    players:{} };
    for (let k in this.players) {
        payload.players[k] = [this.engine.players[k].alive?1:0,
                              this.engine.players[k].position.x,
                              this.engine.players[k].position.y,
                              this.engine.players[k].angle,
                              this.engine.players[k].speed,
                              this.engine.players[k].water];
    }
    return payload;
};

GameServer.prototype.addPlayer = function (client) {
    if (this.numPlayers <= 0) {
        this.startServerUpdates();
        this.startGameTickTimer();
        console.log('start timers');
    }
    for (let k in this.players) {
        this.players[k].emit('join', {id: client.userid});
    }
    this.players[client.userid] = client;
    this.engine.addPlayer(client.userid);
    this.numPlayers += 1;

    this.players[client.userid].emit('connected', this.getConnectPayload(client));
};

GameServer.prototype.delPlayer = function (client) {
    this.numPlayers -= 1;
    if (this.numPlayers <= 0) {
        this.stopServerUpdates();
        this.stopGameTickTimer();
        console.log('shutdown timers');
    }
    delete this.players[client.userid];
    this.engine.delPlayer(client.userid);
    for (let k in this.players) {
        this.players[k].emit('part', {id:client.userid});
    }
};

GameServer.prototype.handlePing = function (client, ts) {
    client.emit('message', 'p'+JSON.stringify(ts));
}

GameServer.prototype.handleReset = function () {
    this.readMap(this.mapFileName);
    for (k in this.engine.players) {
        this.engine.resetPlayer(this.engine.players[k]);
    }
    for (k in this.players) {
        var pl = this.getConnectPayload(this.players[k]);
        this.players[k].emit('reset', pl);
    }
}

GameServer.prototype.onMessage = function (client, m) {
    var p = this.engine.players[client.userid];
    // console.log('message', m);
    if (p) {
        var cmd = m[0];
        var data = JSON.parse(m.slice(1));
        switch (cmd) {
        case 'k':
            this.engine.processPlayerKeys(p, data);
            break;
        case 'p':
            this.handlePing(client, data);
            break;
        case 'r':
            this.handleReset();
            break;
        }
    }
};

GameServer.prototype.startGameTickTimer = function () {
    this.gameTickTimer = setInterval(function() {
        var t = new Date().getTime();
        this.engine.lastTickDuration = t - this.engine.tickTime;
        this.engine.tickTime = t;
        this.engine.stepOneTick();
    }.bind(this), 15);
};

GameServer.prototype.stopGameTickTimer = function () {
    clearInterval(this.gameTickTimer);
}

GameServer.prototype.startServerUpdates = function () {
    this.serverUpdateTimer = setInterval(this.sendServerUpdate.bind(this), 100); // 45
};

GameServer.prototype.stopServerUpdates = function () {
    clearInterval(this.serverUpdateTimer);
}

GameServer.prototype.sendServerUpdate = function () {
    var full = {};
    full.t = this.engine.ticks;
    full.p = {};
    for (let k in this.players) {
        full.p[k] = [this.engine.players[k].alive?1:0,
                     this.engine.players[k].position.x,
                     this.engine.players[k].position.y,
                     this.engine.players[k].angle,
                     this.engine.players[k].speed,
                     this.engine.players[k].turnFlag,
                     this.engine.players[k].water];
    }
    full.m = new Array(this.engine.map.updates.length);
    for (let i=0; i<this.engine.map.updates.length; i++) {
        full.m[i] = [this.engine.map.updates[i], this.engine.map.data[this.engine.map.updates[i]]];
    }

    full.f = this.engine.map.fireUpdates;
    full.r = this.engine.map.retardantUpdates;

    if( this.engine.map.viewPortUpdated ) {
        full.vp = this.engine.map.viewPort;
        this.engine.map.viewPortUpdated = false;
    }

    for (let k in this.players) {
        full.lk = this.engine.players[k].lastKey;
        this.players[k].emit('serverupdate', full);
    }

    this.engine.map.updates.length = 0;
    this.engine.map.fireUpdates.length = 0;
    this.engine.map.retardantUpdates.length = 0;
};

exports.GameServer = GameServer;
