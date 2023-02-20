/**
 * @license
 * Copyright 2018,2019 Shawn Betts
 * SPDX-License-Identifier: MIT
**/

var engine = require('./gameengine');
var config = require('./config');

function GameServer(logging, consolefn) {
    this.consolefn = consolefn;
    this.gameNumber = 0;
    this.mode = 'lobby';
    this.logging = logging;
}

GameServer.prototype = {};

GameServer.prototype.reset = function () {
    this.players = [];
    this.tickTime = 0;
    this.clientGameNumbers = {};
    // Set everything up for a fresh new game
    this.engine = new engine.GameEngine(new config.Config());
    this.engine.placeFortresses(10, 50);
    // this.engine.placeAsteroids(10, 50);
    this.engine.placeWalls();
    this.engine.setStartLocations(2);
};

GameServer.prototype.addPlayer = function (client, joinData) {
    this.clientGameNumbers[client.userid] = joinData.gnum;
    for (let i=0; i<this.players.length; i++) {
        this.players[i].client.emit('join', {id: client.userid});
    }
    this.players.push({client: client,
                       id: client.userid});
    this.engine.addPlayer(client.userid);

    var players = new Array(this.players.length);
    for (let i=0; i<this.players.length; i++) {
        players[i] = {color: this.engine.players[i].color,
                      id: this.engine.players[i].id};
    }
    var fpayload = new Array(this.engine.fortresses.length);
    for (let i =0;i<this.engine.fortresses.length; i++) {
        fpayload[i] = [this.engine.fortresses[i].alive?1:0,
                       this.engine.fortresses[i].position.x,
                       this.engine.fortresses[i].position.y,
                       this.engine.fortresses[i].angle,
                       this.engine.fortresses[i].radius];
    }
    client.emit('joined', {id:client.userid,
                           players: players,
                           fortresses: fpayload,
                           asteroids: this.engine.asteroids,
                           walls: this.engine.walls,
                           spheres: this.engine.spheres,
                           startLocations: this.engine.startLocations});

    this.consolefn('num players', this.players.length);
    if (this.players.length >= 2) {
        this.countDownToStart(5);
    }
};

GameServer.prototype.countDownToStart = function (nsecs) {
    if (nsecs > 0) {
        for (let i=0; i<this.players.length; i++)
            this.players[i].client.emit('starting', {seconds: nsecs});
        setTimeout(this.countDownToStart.bind(this), 1000, nsecs-1);
    } else {
        this.startGameMode();
    }
};

GameServer.prototype.disconnected = function (client) {
    this.consolefn("someone disconnected. abort game.");
    if (this.mode === 'game') this.endGameMode();
};

GameServer.prototype.delPlayer = function (client) {
    for (i=0; i<this.players.length;i++) {
        if (this.players[i].id === client.id) {
            this.players.splice(i,1);
            break;
        }
    }
    this.engine.delPlayer(client.userid);
    for (let i=0; i<this.players.length;i++) {
        this.players[i].client.emit('part', {id:client.userid});
    }
};

GameServer.prototype.startGameMode = function () {
    this.mode = 'game';
    this.gameNumber += 1;
    this.startGameTimers();
    this.consolefn('game mode');
};

GameServer.prototype.endGameMode = function () {
    this.mode = 'lobby';
    this.stopGameTimers();
    this.logging.endGame(this.gameNumber,
                         {points: this.engine.points,
                          rawPoints: this.engine.rawPoints});
    for (let i=0; i<this.players.length; i++) {
        this.players[i].client.emit('end');
        this.engine.delPlayer(this.players[i].id);
    }
    this.reset();
    this.consolefn('lobby mode');
};

GameServer.prototype.handlePing = function (client, ts) {
    client.emit('message', 'p'+JSON.stringify(ts));
}

GameServer.prototype.onMessage = function (client, m) {
    var p = this.engine.getPlayer(client.userid);
    // this.consolefn('message', m);
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

GameServer.prototype.startGameTimers = function () {
    var players = new Array(this.engine.players.length);
    for (let i=0; i<this.players.length; i++) {
        players[i] = {id: this.engine.players[i].id,
                      color: this.engine.players[i].color,
                      position: this.engine.players[i].position,
                      angle: this.engine.players[i].angle}
    }
    var fortresses = new Array(this.engine.fortresses.length);
    for (let i=0; i<this.engine.fortresses.length; i++) {
        fortresses[i] = {angle:this.engine.fortresses[i].angle,
                         radius:this.engine.fortresses[i].radius,
                         position: this.engine.fortresses[i].position}
    }
    var asteroids = new Array(this.engine.asteroids.length);
    for (let i=0; i<this.engine.asteroids.length; i++) {
        asteroids[i] = this.engine.asteroids[i];
    }

    this.consolefn('game number', this.gameNumber);
    this.logging.startGame(this.gameNumber, {gnum: this.gameNumber,
                                             client_gnums: this.clientGameNumbers,
                                             titles: this.engine.gameStateColumnTitles(),
                                             players: players,
                                             fortresses: fortresses,
                                             asteroids: asteroids,
                                             startLocations: this.engine.startLocations
                                            });
    this.gameTickTimer = setInterval(this.stepGame.bind(this), 15);
    this.serverUpdateTimer = setInterval(this.sendServerUpdate.bind(this), 45);
};

GameServer.prototype.stepGame = function () {
    var t = new Date().getTime();
    if (this.tickTime) this.lastTickDuration = t - this.tickTime;
    else this.lastTickDuration = 0;
    this.tickTime = t;
    this.engine.stepOneTick(this.lastTickDuration);
    this.logging.saveGameState(this.gameNumber, this.engine.ticks, this.engine.dumpState());
    this.checkForGameEnd();
};

GameServer.prototype.checkForGameEnd = function () {
    if (this.engine.ticks >= this.engine.config.maxTicks) {
        this.endGameMode();
    }
};

GameServer.prototype.stopGameTimers = function () {
    clearInterval(this.gameTickTimer);
    clearInterval(this.serverUpdateTimer);
}

GameServer.prototype.sendServerUpdate = function () {
    var full = {};
    full.p = new Array(this.players.length);
    for (let i=0;i<this.players.length;i++) {
        full.p[i] = [this.engine.players[i].alive?1:0,
                     this.engine.players[i].position.x,
                     this.engine.players[i].position.y,
                     this.engine.players[i].angle,
                     this.engine.players[i].velocity.x,
                     this.engine.players[i].velocity.y,
                     this.engine.players[i].turnFlag];
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
        full.m[i] = [this.engine.missiles[i].id,
                     this.engine.missiles[i].position.x,
                     this.engine.missiles[i].position.y,
                     this.engine.missiles[i].velocity.x,
                     this.engine.missiles[i].velocity.y,
                     this.engine.missiles[i].angle];
    }
    full.s = new Array(this.engine.shells.length);
    for (let i =0;i<this.engine.shells.length; i++) {
        full.s[i] = [this.engine.shells[i].id,
                     this.engine.shells[i].position.x,
                     this.engine.shells[i].position.y,
                     this.engine.shells[i].angle];
    }
    full.a = new Array(this.engine.asteroids.length);
    for (let i =0;i<this.engine.asteroids.length; i++) {
        full.a[i] = [this.engine.asteroids[i].position.x,
                     this.engine.asteroids[i].position.y,
                     this.engine.asteroids[i].angle];
    }
    full.spheres = new Array(this.engine.spheres.length);
    for (let i =0;i<this.engine.spheres.length; i++) {
        full.spheres[i] = [this.engine.spheres[i].id,
                           this.engine.spheres[i].position.x,
                           this.engine.spheres[i].position.y,
                           this.engine.spheres[i].velocity.x,
                           this.engine.spheres[i].velocity.y,
                           this.engine.spheres[i].target ? this.engine.getPlayerIndex(this.engine.spheres[i].target):null];
    }

    full.msg = this.engine.messages;
    full.points = this.engine.points;
    for (let i=0; i<this.players.length; i++) {
        full.lmr = this.engine.players[i].lastMovementRequest;
        this.players[i].client.emit('serverupdate', full);
    }
    this.engine.messages.length = 0;
};

exports.GameServer = GameServer;
