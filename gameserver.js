var engine = require('./gameengine');
var config = require('./config');

var fs = require('fs');
var PNG = require('pngjs').PNG;

function GameServer() {
    this.numConnected = 0;
    this.observers = {};
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
    this.engine.map.viewPort = {x: Math.floor(png.width/2)-50, y:Math.floor(png.height/2)-50, w:100, h:100};

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

GameServer.prototype.getConnectPayload = function (userid) {
    var payload = { id: userid,
                    map: this.engine.map,
                    players:{} };
    for (let k in this.players) {
        payload.players[k] = [this.engine.players[k].alive?1:0,
                              this.engine.players[k].position.x,
                              this.engine.players[k].position.y,
                              this.engine.players[k].angle,
                              this.engine.players[k].speed,
                              this.engine.players[k].turnFlag,
                              this.engine.players[k].water];
    }
    return payload;
};

GameServer.prototype.addPlayer = function (client, data) {
    var _this = this;
    // client.off('mode');
    console.log('add', data.mode, client.userid);
    if (data.mode === 'player') {
        for (let k in this.players) {
            this.players[k].emit('join', {id: client.userid});
        }
        for (let k in this.observers) {
            this.observers[k].emit('join', {id: client.userid});
        }
        this.players[client.userid] = client;
        this.engine.addPlayer(client.userid);
        this.numConnected += 1;

        this.players[client.userid].on('movementRequest', function(m) { _this.handleMovementRequest(client, m); });
        this.players[client.userid].on('reset', function(m) { _this.handleReset(); });

        this.players[client.userid].on('disconnect', function () {
            console.log('\t socket.io:: player ' + client.userid + ' disconnected');
            _this.delPlayer(client);
        });

        this.players[client.userid].emit('start', this.getConnectPayload(client.userid));
    } else if (data.mode === 'observer') {
        this.observers[client.userid] = client;
        this.numConnected += 1;
        this.observers[client.userid].on('reset', function(m) { _this.handleReset(); });
        this.observers[client.userid].on('disconnect', function () {
            console.log('\t socket.io:: observer ' + client.userid + ' disconnected');
            _this.delObserver(client);
        });
        this.observers[client.userid].emit('start', this.getConnectPayload());
    } else {
        console.log('unknown mode "'+data.mode+'" from client '+client.userid);
    }

    if (this.numConnected === 1) {
        this.startServerUpdates();
        this.startGameTickTimer();
        console.log('start timers');
    }
};

GameServer.prototype.delPlayer = function (client) {
    this.numConnected -= 1;
    if (this.numConnected <= 0) {
        this.stopServerUpdates();
        this.stopGameTickTimer();
        console.log('shutdown timers');
    }
    delete this.players[client.userid];
    this.engine.delPlayer(client.userid);
    for (let k in this.players) {
        this.players[k].emit('part', {id:client.userid});
    }
    for (let k in this.observers) {
        this.observers[k].emit('part', {id:client.userid});
    }
};

GameServer.prototype.delObserver = function (client) {
    this.numConnected -= 1;
    if (this.numConnected <= 0) {
        this.stopServerUpdates();
        this.stopGameTickTimer();
        console.log('shutdown timers');
    }
    delete this.observers[client.userid];
};

GameServer.prototype.handlePing = function (client, ts) {
    if (this.engine.players[client.userid]) {
        console.log('ping from client', ts);
        client.emit('ping', ts);
    }
}

GameServer.prototype.handleReset = function () {
    this.engine.map.fire.length = 0;
    this.engine.map.fireUpdates.length = 0;
    this.engine.map.wayPoints.length = 0;
    this.engine.map.wayPointUpdates.length = 0;
    this.engine.map.retardant.length = 0;
    this.engine.map.retardantUpdates.length = 0;
    this.engine.map.timeout = this.engine.config.map.resizeDuration;
    this.readMap(this.mapFileName);

    for (k in this.engine.players) {
        this.engine.resetPlayer(this.engine.players[k]);
    }
    var pl = this.getConnectPayload();
    for (k in this.players) {
        pl.id = k;
        this.players[k].emit('reset', pl);
    }
    pl.id = undefined;
    for (k in this.observers)
        this.observers[k].emit('reset', pl);
};


GameServer.prototype.handleMovementRequest = function (client, data) {
    var p = this.engine.players[client.userid];
    if (p) {
        this.engine.processPlayerKeys(p, data);
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
    full.wp = this.engine.map.wayPointUpdates;

    if( this.engine.map.viewPortUpdated ) {
        full.vp = this.engine.map.viewPort;
        this.engine.map.viewPortUpdated = false;
    }

    for (let k in this.players) {
        full.lk = this.engine.players[k].lastKey;
        this.players[k].emit('serverUpdate', full);
    }

    full.lk = undefined;
    for (let k in this.observers) {
        this.observers[k].emit('serverUpdate', full);
    }

    this.engine.map.updates.length = 0;
    this.engine.map.fireUpdates.length = 0;
    this.engine.map.retardantUpdates.length = 0;
    this.engine.map.wayPointUpdates.length = 0;
};

exports.GameServer = GameServer;
