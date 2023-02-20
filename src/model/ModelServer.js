/**
 * @license
 * Copyright 2018,2019 Shawn Betts
 * SPDX-License-Identifier: MIT
**/

const net = require('net');
const client = require('./client');
// Experiment = require("./experiment").Experiment;

var io = require('socket.io-client');

var defaultHost = '';
var defaultPort = 3000;

function json2sexp(json) {
    if (Array.isArray(json)) {
        var ret = '(';
        for (var i=0; i<json.length; i++) {
            if (i>=1) ret += ' ';
            ret += json2sexp(json[i]);
        }
        return ret + ')';
    } else if (typeof json == 'object') {
        var ret = '(';
        var first = true;
        for (var k in json) {
            if (json.hasOwnProperty(k)) {
                if (first) first = false;
                else ret += ' ';
                ret += ':' + k.toString();
                ret += ' ' + json2sexp(json[k]);
            }
        }
        return ret + ')';
    } else if (typeof json === 'string') {
        // special case for keyword symbols
        if (json[0] === ':')
            return json;
        else
            return '"' + json + '"';
    } else if (json === Infinity) {
        return 'inf';
    } else if (json === true) {
        return 'T';
    } else if (json === false) {
        return 'NIL';
    } else if (json === null) {
        console.log('null');
        return 'NIL';
    } else {
        return json.toString();
    }
}

function Connection (url, name) {
    client.Client.call(this, 1, name);
    this.socket = io.connect(url);
}

Connection.prototype = Object.create(client.Client.prototype);

Connection.prototype.startConnection = function () {
    client.Client.prototype.connect.call(this, this.socket);
};


Connection.prototype.getGameState = function () {
    console.log('getGameState', this.status);
    if (this.status === 'identification' ||
        this.status === 'countdown') {
        return {screen: 'waiting'};
    } else if (this.status === 'game') {
        var state = this.engine.dumpState();
        state.screen = 'game';
        return state;
    } else if (this.status === 'finished') {
        return {screen: 'score',
                points: this.engine.points,
                rawPoints: this.engine.rawPoints};
    } else {
        return {screen: 'disconnected'};
    }
};

Connection.prototype.isFinished = function () {
    return this.status === 'finished';
};

Connection.prototype.pressKey = function (k) {
    client.Client.prototype.pressKey.call(this, this.keyNameToCode(k));
};

Connection.prototype.releaseKey = function (k) {
    client.Client.prototype.releaseKey.call(this, this.keyNameToCode(k));
};

Connection.prototype.updateStatus = function (status) {
    client.Client.prototype.updateStatus.call(this, status);
    if (status === 'game') {
        console.log('start update timer');
        this.updateid = setInterval(this.update.bind(this), 1000/60.0);
    } else if (status === 'finished') {
        console.log('shutdown update timer');
        clearInterval(this.updateid);
    }
};


function ModelInterface (socket, name, opts) {
    this.sock = socket;
    this.name = name;
    this.lineEnding = opts.lineEnding || '\r\n';
    this.mode = 'config';
    this.commands = [];
    this.logFormat = opts.logFormat || 'native';
    this.gnum = 0;
    this.drawing = true;
}

ModelInterface.prototype = {
    // GUI version must implement these functions
    logMsg: function (dest, msg) { console.log(dest, msg); },
    drawingChanged: function () { },
    initGameCanvas: function () { },
    drawScoreScreen: function () { },
    drawGame: function () { },
    updateSubjectID: function (id) { },
    shutdownGUI: function () { },

    greet: function () {
        this.logMsg ('server', 'new connection from ' + this.sock.remoteAddress + ':' + this.sock.remotePort);
        this.logMsg (this.name, 'Config');

        this.sock.write(json2sexp({'screen-type': 'config',
                                   id: this.name,
                                   logFormat: this.logFormat}) + this.lineEnding);
    },

    parseData: function (data) {
        // console.log ('data: ' + data);
        var lines = data.split(/[\n\r]+/);
        // console.log('lines', lines);
        for(var i=0; i<lines.length; i++) {
            lines[i] = lines[i].trim();
            if (lines[i].length > 0) this.commands.push(lines[i]);
        }
    },

    decodeKey: function (str) {
        if (str === 'fire' || str === '32') return 'fire';
        else if (str === 'thrust' || str === '119') return 'thrust';
        else if (str === 'left' || str === '97') return 'left';
        else if (str === 'right' || str === '100') return 'right';
        else return null;
    },

    setDrawing: function(val) {
        if (val === '0') {
            this.drawing = false;
        } else {
            this.drawing = true;
        }
        if (this.mode === 'game') this.drawingChanged();
    },

    startGame: function () {
        this.mode = 'game';
        this.gnum += 1;
        this.connection = new Connection('ws://localhost:3100', this.name);
        this.connection.startConnection();
        this.initGameCanvas();
        this.drawGame();
        this.logMsg (this.name, 'Start Game ' + this.gnum.toString());
    },

    handleCommand: function (data) {
        // console.log('cmd', data);
        if (this.mode === 'config') {
            var line = data.split(' ');
            var cmd = line[0];
            var error = null;
            if (cmd.toLowerCase() === 'id') {
                if (line.length >= 2) {
                    this.updateSubjectID(line[1]);
                    this.name = line[1];
                }
                else error = cmd + ': needs one argument';
            } else if (cmd.toLowerCase() === 'condition') {
                if (line.length >= 2) {
                    error = cmd + ': conditions are currently not supported.';
                } else {
                    error = cmd + ': needs one argument';
                }
            } else if (cmd.toLowerCase() === 'quit') {
                this.shutdown('client has quit.');
                return;
            } else if (cmd.toLowerCase() === 'drawing') {
                if (line.length >= 2) this.setDrawing(line[1]);
                else error = cmd + ': needs one argument';
            } else if (cmd.toLowerCase() === 'continue') {
                this.logMsg ('server', this.name + ' is starting');
                this.mode = 'game';
                this.startGame();
                return;
            } else if (cmd.toLowerCase() === 'logformat') {
                if (line.length >= 2) this.logFormat = line[1];
                else error = cmd + ': needs one argument';
            } else if (cmd === 'config') {
                if (line.length >= 3) {
                    var key = line[1];
                    var val = line[2];
                    error = cmd + ': unknown setting ' + key;
                } else {
                    error = cmd + ': needs 2 arguments';
                }
            } else {
                error = 'unknown command: ' + cmd;
            }
            this.sock.write(json2sexp({'screen-type': 'config',
                                       id: this.name,
                                       logFormat: this.logFormat,
                                       result: error === null,
                                       error: error}) + this.lineEnding);
        } else if (this.mode === 'game') {
            var line = data.split(' ');
            var cmd = line[0];
            if (cmd.toLowerCase() === 'quit') {
                this.shutdown('client has quit.');
                return;
            } else if (cmd === 'drawing') {
                if (line.length >= 2) this.setDrawing(line[1]);
            } else if (cmd.toLowerCase() === 'keydown') {
                if (line.length >= 2) {
                    var k = this.decodeKey(line[1]);
                    if (k) this.connection.pressKey(k);
                }
            } else if (cmd.toLowerCase() === 'keyup') {
                if (line.length >= 2) {
                    var k = this.decodeKey(line[1]);
                    if (k) this.connection.releaseKey(k);
                }
            } else if (cmd.toLowerCase() === 'state') {
                this.drawGame();
                this.sock.write(json2sexp(this.connection.getGameState()) + this.lineEnding);
                if (this.connection.status === 'finished') this.mode === 'score';
            } else {
                // ignore garbage.
            }
        } else if (this.mode === 'score') {
            var line = data.split(' ');
            var cmd = line[0];
            if (cmd.toLowerCase() === 'quit') {
                this.shutdown('client has quit.');
                return;
            } else if (cmd.toLowerCase() === 'continue') {
                this.startGame();
                return;
            }
            // this.sendScore();
        }
    },

    handleData: function (data) {
        this.parseData(data);
        for(var i=0; i<this.commands.length; i++) {
            this.handleCommand(this.commands[i]);
        }
        this.commands = [];
    },

    shutdown: function (msg) {
        if (!this.hasShutdown) {
            this.hasShutdown = true;
            this.sock.end();
            this.sock.destroy();
            this.logMsg('server', this.name + ': ' + msg);
            this.shutdownGUI();
        } else {
            console.log('ignore multiple shutdowns.');
        }
    },

    disconnect: function () {
        this.shutdown('client has disconnected.');
    }
};


var clientNum = 0;

function startServer (logFn, clientFn, opts) {
    var port = opts.port || defaultPort;
    net.createServer (function (sock) {
        sock.setEncoding('utf8');
        sock.setNoDelay(true);

        clientNum += 1;
        var client = clientFn(sock, 'model' + clientNum, {lineEnding: opts.lineEnding});
        client.greet();
        sock.on ('data', function (data) { client.handleData(data); });
        sock.on ('error', function (err) {
            if (err.errno === 'ECONNRESET') { }
            else { console.log('socket error', err); }
        });
        sock.on ('close', function () { client.disconnect(); });
    }).listen (port, defaultHost);
    logFn ('server', 'Listening on port ' + port);
}

exports.ModelInterface = ModelInterface;
exports.startServer = startServer;
