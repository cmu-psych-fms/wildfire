var engine = require('./gameengine');
var config = require('./config');

function GameServer(logging) {
    this.numPlayers = 0;
    this.players = [];
    this.engine = new engine.GameEngine(new config.Config());
    this.engine.placeFortresses(10, 50);
    this.engine.placeAsteroids(10, 50);
    this.mode = 'lobby';
    this.logging = logging;
}

GameServer.prototype = {};

GameServer.prototype.addPlayer = function (client) {
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
    client.emit('connected', {id:client.userid,
                              players: players,
                              fortresses: fpayload,
                              asteroids: this.engine.asteroids});

    console.log('num players', this.players.length);
    if (this.players.length >= 2) {
        setTimeout(this.startGameMode.bind(this), 5000);
    }
};

GameServer.prototype.disconnected = function (client) {
    console.log("someone disconnected. abort game.");
    if (this.mode === 'game') this.endGameMode();
};

GameServer.prototype.delPlayer = function (client) {
    this.numPlayers -= 1;
    // if (this.numPlayers <= 0) {
    //     console.log('No more players');
    //     this.endGameMode();
    // }
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
    this.startServerUpdates();
    this.startGameTickTimer();
    console.log('game mode');
};

GameServer.prototype.endGameMode = function () {
    this.mode = 'lobby';
    this.stopServerUpdates();
    this.stopGameTickTimer();
    this.logging.endGame({points: this.engine.points,
                          rawPoints: this.engine.rawPoints});
    for (let i=0; i<this.players.length; i++) {
        this.players[i].client.emit('end');
        this.engine.delPlayer(this.players[i].id);
    }
    this.players = [];
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
    var p = this.engine.getPlayer(client.userid);
    // console.log('message', m);
    if (p) {
        var cmd = m[0];
        var data = JSON.parse(m.slice(1));
        switch (cmd) {
        case 'g':
            console.log('got game number', data);
            this.game_number = data.gnum;
            break;
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
    var players = [];
    var fortresses = [];
    var asteroids = [];
    console.log('game number', this.game_number);
    this.logging.startGame(this.game_number, {gnum: this.game_number,
                                              titles: this.engine.gameStateColumnTitles(),
                                              players: players,
                                              fortresses: fortresses,
                                              asteroids: asteroids
                                             });
    this.gameTickTimer = setInterval(function() {
        var t = new Date().getTime();
        if (this.tickTime) this.lastTickDuration = t - this.tickTime;
        else this.lastTickDuration = 0;
        this.tickTime = t;
        this.engine.stepOneTick(this.lastTickDuration);
        this.logging.saveGameState(this.game_number, this.engine.ticks, this.engine.dumpState());
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
    full.points = this.engine.points;
    for (let i=0; i<this.players.length; i++) {
        full.lmr = this.engine.players[i].lastMovementRequest;
        this.players[i].client.emit('serverupdate', full);
    }
    this.engine.messages.length = 0;
};

exports.GameServer = GameServer;
