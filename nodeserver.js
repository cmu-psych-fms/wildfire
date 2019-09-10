var verbose = false,
    gameport = 3000,
    uuid = require('uuid/v1'),
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
server.readMap(path.join(__dirname, 'sprites/bigmap.png'));

sio.sockets.on('connection', function (client) {
    //Generate a new UUID, looks something like
    //5b2ca132-64bd-4513-99da-90e838ca47d1
    //and store this on their socket/connection
    client.userid = uuid();

    //Useful to know when someone connects
    console.log('\t socket.io:: player ' + client.userid + ' connected');

    // Wait for the player to announce what mode they want to use
    client.on('greet', function (m) { server.addPlayer(client, m); });

});
