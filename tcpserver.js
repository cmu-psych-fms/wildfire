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
};

Handler.prototype.onEnd = function () {
    this.socket.end();
};

Handler.prototype.onClose = function () {
    this.emitter.emit('disconnect');
};

Handler.prototype.onData = function (data) {
    this.buffer += data;

    var idx = this.buffer.indexOf(this.separator);

    if (idx >= 0) {
        var raw = this.buffer.slice(0,idx);
        console.log('data', raw.trim());
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
}

Server.prototype = {};

Server.prototype.listen = function(port) {
    this.server = net.createServer(this.onConnect.bind(this));
    this.server.listen(port);
};

Server.prototype.onConnect = function(socket) {
    var remoteAddress = socket.remoteAddress + ':' + socket.remotePort;
    console.log('TCP client:', remoteAddress);

    var h = new Handler(socket);
    h.addListeners();

    this.emitter.emit('connection', h);
};

exports.Server = Server;
