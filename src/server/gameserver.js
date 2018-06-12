var engine = require('./gameengine');
var config = require('./config');

function GameServer() {
    this.numPlayers = 0;
    this.players = {};
    this.engine = new engine.GameEngine(new config.Config());
    this.engine.placeFortresses(10, 50);
    this.engine.placeAsteroids(10, 50);
    this.mode = 'lobby';
}

GameServer.prototype = {};

GameServer.prototype.addPlayer = function (client) {
    for (let k in this.players) {
        this.players[k].emit('join', {id: client.userid});
    }
    this.players[client.userid] = client;
    this.engine.addPlayer(client.userid);
    this.numPlayers += 1;

    var players = {};
    for (let k in this.players) {
        players[k] = {color: this.engine.players[k].color};
    }
    var fpayload = new Array(this.engine.fortresses.length);
    for (let i =0;i<this.engine.fortresses.length; i++) {
        fpayload[i] = [this.engine.fortresses[i].alive?1:0,
                       this.engine.fortresses[i].position.x,
                       this.engine.fortresses[i].position.y,
                       this.engine.fortresses[i].angle,
                       this.engine.fortresses[i].radius];
    }
    this.players[client.userid].emit('connected', {id:client.userid,
                                                   players: players,
                                                   fortresses: fpayload,
                                                   asteroids: this.engine.asteroids});

    console.log('num players', this.numPlayers);
    if (this.numPlayers >= 2) {
        setTimeout(this.startGameMode.bind(this), 5000);
    }
};

GameServer.prototype.delPlayer = function (client) {
    this.numPlayers -= 1;
    if (this.numPlayers <= 0) {
        console.log('No more players');
        this.endGameMode();
    }
    delete this.players[client.userid];
    this.engine.delPlayer(client.userid);
    for (let k in this.players) {
        this.players[k].emit('part', {id:client.userid});
    }
};

GameServer.prototype.startGameMode = function () {
    this.mode = 'game';
    this.startServerUpdates();
    this.startGameTickTimer();
    console.log('game mode');
};

GameServer.prototype.endGameMode = function () {
    this.mode = 'lobby';
    this.stopServerUpdates();
    this.stopGameTickTimer();
    for (let k in this.players) {
        this.players[k].emit('end');
        this.engine.delPlayer(k);
    }
    this.players = {};
    // Set everything up for a fresh new game
    this.engine = new engine.GameEngine(new config.Config());
    this.engine.placeFortresses(10, 50);
    this.engine.placeAsteroids(10, 50);
    console.log('lobby mode');
};

GameServer.prototype.handlePing = function (client, ts) {
    client.emit('message', 'p'+JSON.stringify(ts));
}

GameServer.prototype.onMessage = function (client, m) {
    var p = this.engine.players[client.userid];
    // console.log('message', m);
    if (p) {
        var cmd = m[0];
        var data = JSON.parse(m.slice(1));
        switch (cmd) {
        case 'k':
            this.engine.accumPlayerMovementRequests(p, data);
            break;
        case 'p':
            this.handlePing(client, data);
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
        this.checkForGameEnd();
    }.bind(this), 15);
};

GameServer.prototype.checkForGameEnd = function () {
    if (this.engine.ticks >= this.engine.config.maxTicks) {
        this.endGameMode();
    }
};

GameServer.prototype.stopGameTickTimer = function () {
    clearInterval(this.gameTickTimer);
}

GameServer.prototype.startServerUpdates = function () {
    this.serverUpdateTimer = setInterval(this.sendServerUpdate.bind(this), 45);
};

GameServer.prototype.stopServerUpdates = function () {
    clearInterval(this.serverUpdateTimer);
}

GameServer.prototype.sendServerUpdate = function () {
    var full = {};
    full.p = {};
    for (let k in this.players) {
        full.p[k] = [this.engine.players[k].alive?1:0,
                     this.engine.players[k].position.x,
                     this.engine.players[k].position.y,
                     this.engine.players[k].angle,
                     this.engine.players[k].velocity.x,
                     this.engine.players[k].velocity.y,
                     this.engine.players[k].turnFlag];
    }
    full.f = new Array(this.engine.fortresses.length);
    for (let i =0;i<this.engine.fortresses.length; i++) {
        full.f[i] = [this.engine.fortresses[i].alive?1:0,
                       this.engine.fortresses[i].position.x,
                       this.engine.fortresses[i].position.y,
                       this.engine.fortresses[i].angle];
    }
    full.m = new Array(this.engine.missiles.length);
    for (let i =0;i<this.engine.missiles.length; i++) {
        full.m[i] = [this.engine.missiles[i].position.x,
                     this.engine.missiles[i].position.y,
                     this.engine.missiles[i].velocity.x,
                     this.engine.missiles[i].velocity.y,
                     this.engine.missiles[i].angle];
    }
    full.s = new Array(this.engine.shells.length);
    for (let i =0;i<this.engine.shells.length; i++) {
        full.s[i] = [this.engine.shells[i].position.x,
                     this.engine.shells[i].position.y,
                     this.engine.shells[i].angle];
    }
    full.a = new Array(this.engine.asteroids.length);
    for (let i =0;i<this.engine.asteroids.length; i++) {
        full.a[i] = [this.engine.asteroids[i].position.x,
                     this.engine.asteroids[i].position.y,
                     this.engine.asteroids[i].angle];
    }
    full.msg = this.engine.messages;
    for (let k in this.players) {
        full.lmr = this.engine.players[k].lastMovementRequest;
        this.players[k].emit('serverupdate', full);
    }
    this.engine.messages.length = 0;
};

exports.GameServer = GameServer;
