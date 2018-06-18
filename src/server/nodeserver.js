// var verbose = false,
//     gameport = 3000,
//     path = require('path'),
//     uuid = require('uuid/v1'),
//     app = require('express')(),
//     server = require('http').createServer(app),
//     io = require('socket.io')(server),
//     logging = require('./logging');

// var bodyParser = require('body-parser');
// var urlencodedParser = bodyParser.urlencoded({ extended: false });

const args = require('yargs')
      .option('data', {
          describe: "Specify the location of data files",
          default: "data"})
      .option('client', {
          describe: "Specify the location of client files",
          default: "../browser"})
      .help()
      .argv;

var es = require('./experiment_server');

var server = new es.ExperimentServer(args.data, args.client);



// var Log = new logging.Logging(args.data);
// Log.openDB();

// server.listen(gameport);

// app.get( '/', function( req, res ){
//     if (verbose) console.log('Sending %s', path.join(__dirname, args.client, 'index.html'));
//     res.sendFile( '/index.html' , { root:path.join(__dirname, args.client) });
// });

// app.post( '/api', urlencodedParser, function( req, res ) {
// // app.get( '/api', function( req, res ) {
//     // var body = req.query;
//     var body = req.body;

//     // console.log(body);

//     if (body.action === 'store-log-block') {
//         Log.addLogBlock(body.worker_id,
//                         body.log,
//                         body.session_id,
//                         body.sync_id);
//         res.end(JSON.stringify({success:true}));
//     } else if (body.action === 'store-game-data') {
//         Log.addGameLog(body.worker_id,
//                        body.log,
//                        body.session_id,
//                        body.game_number);
//         res.end(JSON.stringify({success:true}));
//     } else if (body.action === 'store-log') {
//         Log.addLogBlock(body.worker_id,
//                         body.log);
//         res.end(JSON.stringify({success:true}));
//     } else if (body.action === 'store-progress') {
//         Log.updateProgress(body.worker_id,
//                            body.idx,
//                            body.reward,
//                            body.condition,
//                            body.extra,
//                            body.session_id);
//         res.end(JSON.stringify({success:true}));
//     } else if (body.action === 'resume') {
//         Log.getResume(body.worker_id,
//                       function (r) {
//                           res.end(JSON.stringify(r));
//                       });
//     } else {
//         res.end(JSON.stringify({success:false,
//                                 reason:'unknown action '+body.action}));
//     }
// });

// app.get( '/*' , function( req, res, next ) {

//     //This is the current file they have requested
//     var file = req.params[0];

//     //For debugging, we can track what files are requested.
//     if(verbose) console.log('Sending %s', path.join(__dirname,args.client,file));

//     //Send the requesting client the file.
//     res.sendFile( '/' + file, {root:path.join(__dirname, args.client)} );

// });


// var sio = io.listen(server);

// console.log("Listening on port", gameport);

// // sio.configure(function (){

// //     sio.set('log level', 0);

// //     sio.set('authorization', function (handshakeData, callback) {
// //         callback(null, true); // error first callback style
// //     });

// // });

// var game_server = require('./gameserver.js');

// var server = new game_server.GameServer(Log);

// sio.sockets.on('connection', function (client) {
//     //Generate a new UUID, looks something like
//     //5b2ca132-64bd-4513-99da-90e838ca47d1
//     //and store this on their socket/connection
//     client.userid = uuid();

//     //Useful to know when someone connects
//     console.log('\t socket.io:: player ' + client.userid + ' connected');

//     //tell the player they connected, giving them their id
//     server.addPlayer(client);

//     client.on('message', function(m) {
//         server.onMessage(client, m);
//     });

//     client.on('disconnect', function () {
//         console.log('\t socket.io:: player ' + client.userid + ' disconnected');
//         server.delPlayer(client);
//     });
// });
