var verbose = false,
    gameport = 3000,
    uuid = require('uuid/v1'),
    app = require('express')(),
    server = require('http').createServer(app),
    io = require('socket.io')(server);


server.listen(gameport);

app.get( '/', function( req, res ){
    if (verbose) console.log('Sending %s', __dirname + '/index.html');
    res.sendFile( '/index.html' , { root:__dirname });
});

app.get( '/*' , function( req, res, next ) {

    //This is the current file they have requested
    var file = req.params[0];

    //For debugging, we can track what files are requested.
    if(verbose) console.log('Sending %s', __dirname + '/' + file);

    //Send the requesting client the file.
    res.sendFile( '/' + file, {root:__dirname} );

});


var sio = io.listen(server);

console.log("Listening on port", gameport);

// sio.configure(function (){

//     sio.set('log level', 0);

//     sio.set('authorization', function (handshakeData, callback) {
//         callback(null, true); // error first callback style
//     });

// });

var game_server = require('./gameserver.js');

var server = new game_server.GameServer();

// server.readMap('sprites/map.png');
server.readMap('sprites/bigmap.png');

sio.sockets.on('connection', function (client) {
    //Generate a new UUID, looks something like
    //5b2ca132-64bd-4513-99da-90e838ca47d1
    //and store this on their socket/connection
    client.userid = uuid();

    //Useful to know when someone connects
    console.log('\t socket.io:: player ' + client.userid + ' connected');

    //tell the player they connected, giving them their id
    server.addPlayer(client);

    client.on('message', function(m) {
        server.onMessage(client, m);
    });

    client.on('disconnect', function () {
        console.log('\t socket.io:: player ' + client.userid + ' disconnected');
        server.delPlayer(client);
    });
});
