const net = require('net');
const EventEmitter = require('events');

function Handler(socket) {
    this.socket = socket;
    this.emitter = new EventEmitter();
    this.on = this.emitter.on.bind(this.emitter);
    this.removeAllListeners = this.emitter.removeAllListeners.bind(this.emitter);

    this.buffer = "";
}

Handler.prototype = {};

Handler.prototype.separator = '\n';

Handler.prototype.emit = function (event, data) {
    this.socket.write(JSON.stringify([event,data]) + this.separator);
};

Handler.prototype.addListeners = function () {
    this.socket.setNoDelay(true);
    this.socket.on('data', this.onData.bind(this));
    this.socket.on('end', this.onEnd.bind(this));
    this.socket.on('close', this.onClose.bind(this));
    this.socket.on('error', this.onError.bind(this));
};

Handler.prototype.onError = function (e) {
    console.log('TCP error', e.code)
};

Handler.prototype.onEnd = function () {
    this.socket.end();
};

Handler.prototype.onClose = function () {
    this.emitter.emit('disconnect');
};

Handler.prototype.onData = function (data) {
    this.buffer += data;

    while (true) {
        var idx = this.buffer.indexOf(this.separator);
        if (idx < 0) break;

        var raw = this.buffer.slice(0,idx);
        //console.log('data', raw.trim());
        var msg = JSON.parse(raw);
        this.emitter.emit(msg[0], msg[1]);
        this.buffer = this.buffer.slice(idx+1);
    }
}

Handler.prototype.disconnect = function () {
    this.socket.end();
};

function Server() {
    this.emitter = new EventEmitter();
    this.on = this.emitter.on.bind(this.emitter);
    this.sockets = [];
}

Server.prototype = {};

Server.prototype.listen = function(port) {
    this.server = net.createServer(this.onConnect.bind(this));
    this.server.listen(port);
};

Server.prototype.onConnect = function(socket) {
    var remoteAddress = socket.remoteAddress + ':' + socket.remotePort;
    console.log('TCP client:', remoteAddress);

    this.sockets.push(socket);
    socket.on('close', () => { this.onClose(socket); });

    var h = new Handler(socket);
    h.addListeners();

    this.emitter.emit('connection', h);
};

Server.prototype.onClose = function(socket) {
    var idx = this.sockets.indexOf(socket);
    if (idx >= 0)
        this.sockets.splice(idx,1);
};

Server.prototype.close = function() {
    this.server.close();
    if (this.sockets.length > 0) {
        console.log('Closing', this.sockets.length, 'TCP socket(s).');
        for (let i=0; i<this.sockets.length; i++) {
            this.sockets[i].end();
        }
    }
};

exports.Server = Server;
