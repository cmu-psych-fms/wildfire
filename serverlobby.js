const EventEmitter = require('events');
var uuid = require('uuid/v1');
var MersenneTwister = require('mersenne-twister');

const IDLE_TIME = 5 * 60 * 1000;

function Lobby(log) {
    this.log = log;
    this.clients = [];
    this.log.startScreen('lobby', {clients: []});

    this.emitter = new EventEmitter();
    this.on = this.emitter.on.bind(this.emitter);
}

Lobby.prototype = {};

Lobby.prototype.broadcast = function(event, data) {
    for (let i=0; i<this.clients.length; i++) {
        console.log('broadcast', this.clients[i].id, event);
        this.clients[i].socket.emit(event, data);
    }
};

Lobby.prototype.broadcastRoster = function(join, part) {
    var now = new Date().getTime();
    var roster = {clients: new Array(this.clients.length),
                  join: join || undefined,
                  part: part || undefined};
    for (let i=0; i<this.clients.length; i++) {
        roster.clients[i] = {};
        roster.clients[i].id = this.clients[i].id;
        roster.clients[i].mode = this.clients[i].mode;
        roster.clients[i].ready = this.clients[i].ready;
        console.log(now, this.clients[i].activity, IDLE_TIME)
        roster.clients[i].idle = (now - this.clients[i].activity) > IDLE_TIME;
    }
    this.broadcast('roster', roster);
    this.log.lg('roster', roster);
};

Lobby.prototype.addClient = function(socket) {
    var _this = this;
    var c = {id: uuid(),
             socket: socket,
             ready: false,
             activity: new Date().getTime(),
             mode: 'player'};
    this.clients.push(c);
    console.log('add ' + c.id);

    socket.on('mode', function (data) { _this.setMode(c, data);});
    socket.on('disconnect', function () { _this.delClient(c);});
    socket.on('ready', function (data) { _this.setReady(c, data);});
    socket.on('seed', function (data) { _this.seedRNG(data);});

    c.socket.emit('welcome', {id:c.id})
    this.broadcastRoster(c.id);
};

Lobby.prototype.delClient = function(client) {
    for (let i=0; i<this.clients.length; i++) {
        var id = this.clients[i].id;
        if (id === client.id) {
            console.log('del ' + id);
            this.clients.splice(i,1);
            this.broadcastRoster(undefined, id);
            break;
        }
    }
};

Lobby.prototype.setMode = function(client, data) {
    client.activity = new Date().getTime();
    if (data === 'player' || data === 'observer')
        client.mode = data;
    this.broadcastRoster();
};

Lobby.prototype.setReady = function(client, data) {
    var now = new Date().getTime();
    client.activity = now;
    if (data) client.ready = true;
    else client.ready = false;

    var all = true;
    for (let i=0; i<this.clients.length; i++) {
        if ((now - this.clients[i].activity) > IDLE_TIME) {
            console.log('ignoring idler', this.clients[i].id)
            continue;
        }
        if (!this.clients[i].ready) {
            all = false;
            break;
        }
    }

    if (all) {
        this.startGame();
    } else {
        this.broadcastRoster();
    }
};

Lobby.prototype.seedRNG = function(data) {
    // Any client can seed the RNG. But typically, only a model will
    // send a 'seed' event.
    if (typeof data === 'number') {
        console.log('seeding', data);
        var m = new MersenneTwister(data);
        Math.random = m.random.bind(m);
    }
}


Lobby.prototype.removeListenersAllClients = function() {
    for (let i=0; i<this.clients.length; i++) {
        this.clients[i].socket.removeAllListeners('mode');
        this.clients[i].socket.removeAllListeners('disconnect');
        this.clients[i].socket.removeAllListeners('ready');
        this.clients[i].socket.removeAllListeners('seed');
    }
};

Lobby.prototype.startGame = function() {
    this.removeListenersAllClients();
    this.log.endScreen();
    this.emitter.emit('start', this.clients)
    this.clients.length = 0;
};

Lobby.prototype.backFromGame = function(clients) {
    var _this = this;
    var logClients = [];
    for (let i=0; i<this.clients.length; i++) {
        logClients.push({id:this.clients[i].id,
                         mode:this.clients[i].mode,
                         ready:this.clients[i].ready});
    }
    for (let i=0; i<clients.length; i++) {
        let c = {};
        c.id = clients[i].id;
        c.mode = clients[i].mode;
        c.socket = clients[i].socket;
        c.activity = new Date().getTime();
        c.ready = false;
        this.clients.push(c)
        c.socket.on('mode', function (data) { _this.setMode(c, data);});
        c.socket.on('disconnect', function () { _this.delClient(c);});
        c.socket.on('ready', function (data) { _this.setReady(c, data);});
        c.socket.on('seed', function (data) { _this.seedRNG(data);});
        logClients.push({id:c.id, mode:c.mode, ready:c.ready});
    }
    console.log('lobby', this.clients.length);
    this.log.startScreen('lobby', {clients: logClients});
    this.broadcastRoster();
};

exports.Lobby = Lobby;
