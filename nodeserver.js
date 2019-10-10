const args = require('yargs')
      .option('log', {
          describe: "Specify whether or not to save logs",
          type: 'boolean',
          default: false})
      .option('logdir', {
          describe: "The directory used to store log files",
          default: 'logs/'})
      .option('id', {
          describe: "Specify the unique identifier for this experiment session.",
          default: '1',
          type: 'string'})
      .help()
      .argv;

if (args.log) {
    if (args.id === undefined) {
        console.error('You must specify a unique identifier for this session with --id.');
        process.exit(1);
    }
}

var verbose = false,
    gameport = 3000,
    app = require('express')(),
    server = require('http').createServer(app),
    io = require('socket.io')(server),
    path = require('path');

server.listen(gameport);

app.get( '/', function( req, res ){
    if (verbose) console.log('Sending %s', path.join(__dirname, 'index.html'));
    res.sendFile( path.join(__dirname, 'index.html' ));
});

app.get( '/*' , function( req, res, next ) {

    //This is the current file they have requested
    var file = req.params[0];

    //For debugging, we can track what files are requested.
    if(verbose) console.log('Sending %s', path.join(__dirname, file));

    //Send the requesting client the file.
    res.sendFile( path.join(__dirname, file ));

});

// sio.configure(function (){

//     sio.set('log level', 0);

//     sio.set('authorization', function (handshakeData, callback) {
//         callback(null, true); // error first callback style
//     });

// });

// var game_server = require('./gameserver.js');

// var server = new game_server.GameServer();

var logging = require('./serverlog');
var Lobby = require('./serverlobby').Lobby;
var GameServer = require('./gameserver').GameServer;

var log = new logging.Log(args.logdir, args.id);
log.readonly = !args.log;

log.startExperiment({build: 'TODO',
                     date: new Date().toString(),
                     id: args.id
                    });

var global_game;
var global_lobby = new Lobby(log);
// server.readMap('sprites/map.png');
// server.readMap(path.join(__dirname, 'sprites/bigmap.png'));

console.log("Listening on port", gameport, '-- models port', gameport+1);

io.on('connection', function (socket) {
    global_lobby.addClient(socket);
});

var tcpserver = require('./tcpserver');
var tcp = new tcpserver.Server();
tcp.listen(gameport + 1);
tcp.on('connection', function (socket) { global_lobby.addClient(socket); });

global_lobby.on('start', function (clients) {
    global_game = new GameServer(log, 'map.png');
    global_game.on('end', function (clients) {
        global_game = undefined;
        global_lobby.backFromGame(clients);
    });
    global_game.startWithClients(clients);
});

function shutdown() {
    if (global_game) global_game.close();
    server.close();
    io.close();
    tcp.close();
    // process.exit(1);
}

process.on('SIGINT', () => {
    console.log('got SIGINT');
    shutdown();
});

process.once('beforeExit', () => {
    console.log('shutting down');
    log.endExperiment();
});
