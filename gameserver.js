var engine = require('./gameengine');
var config = require('./config');

function GameServer() {
    this.numPlayers = 0;
    this.players = {};
    this.engine = new engine.GameEngine(new config.Config());
}

GameServer.prototype = {};

GameServer.prototype.addPlayer = function (client) {
    if (this.numPlayers <= 0) {
        this.startServerUpdates();
        this.startGameTickTimer();
        console.log('start timers');
    }
    this.players[client.userid] = client;
    this.engine.addPlayer(client.userid);
    this.numPlayers += 1;
    for (let k in this.players) {
        this.players[k].emit('join', {id: client.userid});
    }
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
};

GameServer.prototype.handlePing = function (client, ts) {
    client.emit('message', 'p'+JSON.stringify(ts));
}

GameServer.prototype.onMessage = function (client, m) {
    var p = this.engine.players[client.userid];
    console.log('message', m);
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
    this.serverUpdateTimer = setInterval(this.sendServerUpdate.bind(this), 45);
};

GameServer.prototype.stopServerUpdates = function () {
    clearInterval(this.serverUpdateTimer);
}

GameServer.prototype.sendServerUpdate = function () {
    var payload = {};
    for (let k in this.players) {
        payload[k] = [this.engine.players[k].position.x,
                      this.engine.players[k].position.y,
                      this.engine.players[k].angle];
    }

    for (let k in this.players) {
        this.players[k].emit('serverupdate', payload);
    }
};

exports.GameServer = GameServer;
