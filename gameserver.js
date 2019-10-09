const path = require('path');
const EventEmitter = require('events');
var engine = require('./gameengine');
var config = require('./config');

var fs = require('fs');
var PNG = require('pngjs').PNG;

function GameServer(log, mapfile) {
    this.log = log;
    this.numConnected = 0;
    this.observers = {};
    this.players = {};
    this.engine = new engine.GameEngine(new config.Config());
    this.mapFile = path.join(__dirname, 'sprites/'+mapfile);
    // this.engine.placeFortresses(10, 50);
    // this.engine.placeAsteroids(10, 50);
    this.emitter = new EventEmitter();
    this.on = this.emitter.on.bind(this.emitter);

    GameServer.prototype.readMap.call(this);
}

GameServer.prototype = {};

GameServer.prototype.readMap = function () {
    var data = fs.readFileSync(this.mapFile);
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
                    map: {},
                    players:{} };
    var mapKeys = ['width', 'height', 'retardant', 'fire', 'wayPoints', 'viewPort', 'data'];
    for (let i=0; i<mapKeys.length; i++) {
        payload.map[mapKeys[i]] = this.engine.map[mapKeys[i]];
    }
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

GameServer.prototype.addPlayer = function (id, socket) {
    var _this = this;
    this.players[id] = socket;
    this.engine.addPlayer(id);
    this.players[id].on('movementRequest', function(m) { _this.handleMovementRequest(id, m); });
    this.players[id].on('abort', function(m) { _this.handleAbort(id); });

    this.players[id].on('disconnect', function () {
        console.log('del player ' + id);
        _this.delPlayer(id);
    });
    this.numConnected += 1;
};

GameServer.prototype.addObserver = function (id, socket) {
    var _this = this;

    this.observers[id] = socket;
    this.observers[id].on('abort', function(m) { _this.handleAbort(id); });
    this.observers[id].on('disconnect', function () {
        console.log('del observer ' + id);
        _this.delObserver(id);
    });
    this.numConnected += 1;
};

GameServer.prototype.startWithClients = function (clients) {
    for (let i=0; i<clients.length; i++) {
        if (clients[i].mode == 'observer') {
            this.addObserver(clients[i].id, clients[i].socket);
        } else if (clients[i].mode == 'player') {
            this.addPlayer(clients[i].id, clients[i].socket);
        } else {
            console.log('unknown client mode', clients[i].mode);
        }
    }

    for (let id in this.players)
        this.players[id].emit('start', this.getConnectPayload(id));
    for (let id in this.observers)
        this.observers[id].emit('start', this.getConnectPayload());

    if (this.numConnected >= 1) {
        this.startServerUpdates();
        this.startGameTickTimer();
        console.log('start timers');
    }

    var p = this.getConnectPayload();
    p.observers = Object.keys(this.observers);
    this.log.startScreen('game', p);
}

// GameServer.prototype.addPlayer = function (client, data) {
//     var _this = this;
//     // client.off('mode');
//     console.log('add', data.mode, client.userid);
//     if (data.mode === 'player') {
//         for (let k in this.players) {
//             this.players[k].emit('join', {id: client.userid});
//         }
//         for (let k in this.observers) {
//             this.observers[k].emit('join', {id: client.userid});
//         }
//         this.players[client.userid] = client;
//         this.engine.addPlayer(client.userid);
//         this.numConnected += 1;

//         this.players[client.userid].on('movementRequest', function(m) { _this.handleMovementRequest(client, m); });
//         this.players[client.userid].on('reset', function(m) { _this.handleReset(); });

//         this.players[client.userid].on('disconnect', function () {
//             console.log('del player ' + client.userid);
//             _this.delPlayer(client);
//         });

//         this.players[client.userid].emit('start', this.getConnectPayload(client.userid));
//     } else if (data.mode === 'observer') {
//     } else {
//         console.log('unknown mode "'+data.mode+'" from client '+client.userid);
//     }

//     if (this.numConnected === 1) {
//         this.startServerUpdates();
//         this.startGameTickTimer();
//         console.log('start timers');
//     }
// };

GameServer.prototype.delPlayer = function (id) {
    delete this.players[id];
    this.engine.delPlayer(id);
    for (let k in this.players) {
        this.players[k].emit('part', {id:id});
    }
    for (let k in this.observers) {
        this.observers[k].emit('part', {id:id});
    }
    this.numConnected -= 1;

    if (this.numConnected <= 0) {
        this.shutdown();
    }
};

GameServer.prototype.delObserver = function (id) {
    delete this.observers[id];

    this.numConnected -= 1;
    if (this.numConnected <= 0) {
        this.shutdown();
    }
};

GameServer.prototype.shutdown = function (score) {
    this.stopServerUpdates();
    this.stopGameTickTimer();
    console.log('shutdown timers');
    for (let k in this.players) this.players[k].emit('end', {score: score});
    for (let k in this.observers) this.observers[k].emit('end', {score: score});

    var clients = [];
    for (let k in this.players) {
        this.players[k].removeAllListeners('movementRequest');
        this.players[k].removeAllListeners('abort');
        this.players[k].removeAllListeners('disconnect');
        clients.push({id:k, mode:'player', socket: this.players[k]});
    }
    for (let k in this.observers) {
        this.observers[k].removeAllListeners('abort');
        this.observers[k].removeAllListeners('disconnect');
        clients.push({id:k, mode:'observer', socket: this.observers[k]});
    }

    this.log.endScreen({'score': score, 'abort': score === undefined});
    this.emitter.emit('end', clients);
};

GameServer.prototype.handlePing = function (client, ts) {
    if (this.engine.players[client.userid]) {
        console.log('ping from client', ts);
        client.emit('ping', ts);
    }
}

GameServer.prototype.handleAbort = function (id) {
    console.log('abort', id);
    this.shutdown();
};


GameServer.prototype.handleMovementRequest = function (id, data) {
    var p = this.engine.players[id];
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
        if (this.engine.isFinished()) {
            this.shutdown(this.engine.getScore());
        } else {
            // FIXME: include the state of the game
            this.log.lg('tick');
        }
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
        full.lms = this.engine.players[k].lastKey.seq;
        this.players[k].emit('update', full);
    }

    full.lms = undefined;
    for (let k in this.observers) {
        this.observers[k].emit('update', full);
    }

    this.log.lg('s', full);

    this.engine.map.updates.length = 0;
    this.engine.map.fireUpdates.length = 0;
    this.engine.map.retardantUpdates.length = 0;
    this.engine.map.wayPointUpdates.length = 0;
};

GameServer.prototype.close = function() {
    for (let k in this.players) this.players[k].disconnect(true);
    for (let k in this.observers) this.observers[k].disconnect(true);
    this.stopServerUpdates();
    this.stopGameTickTimer();
};


exports.GameServer = GameServer;
