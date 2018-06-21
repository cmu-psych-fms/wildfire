var verbose = false,
    gameport = 3000,
    path = require('path'),
    uuid = require('uuid/v1'),
    http = require('http'),
    logging = require('./fileLogging'),
    game_server = require('./gameserver.js');

function ExperimentServer(data_dir, client_dir) {
    this.app = require('express')();
    this.server = http.createServer(this.app);
    this.io = require('socket.io')(this.server);

    var bodyParser = require('body-parser');
    var urlencodedParser = bodyParser.urlencoded({ extended: false });

    this.Log = new logging.FileLogging(data_dir);
    this.Log.openDB();

    this.server.listen(gameport);

    this.app.get( '/', function( req, res ){
        if (verbose) console.log('Sending %s', path.join(client_dir, 'index.html'));
        res.sendFile( '/index.html' , { root:path.join(client_dir) });
    });

    this.app.post( '/api', urlencodedParser, function( req, res ) {
        // app.get( '/api', function( req, res ) {
        // var body = req.query;
        var body = req.body;

        // console.log(body);

        if (body.action === 'store-log-block') {
            this.Log.addLogBlock(body.worker_id,
                            body.log,
                            body.session_id,
                            body.sync_id);
            res.end(JSON.stringify({success:true}));
        } else if (body.action === 'store-game-data') {
            this.Log.addGameLog(body.worker_id,
                           body.log,
                           body.session_id,
                           body.game_number);
            res.end(JSON.stringify({success:true}));
        } else if (body.action === 'store-log') {
            this.Log.addSessionLog(body.worker_id, body.log);
            res.end(JSON.stringify({success:true}));
        } else if (body.action === 'store-progress') {
            this.Log.updateProgress(body.worker_id,
                               body.idx,
                               body.reward,
                               body.condition,
                               body.extra,
                               body.session_id);
            res.end(JSON.stringify({success:true}));
        } else if (body.action === 'resume') {
            this.Log.getResume(body.worker_id,
                          function (r) {
                              res.end(JSON.stringify(r));
                          });
        } else {
            res.end(JSON.stringify({success:false,
                                    reason:'unknown action '+body.action}));
        }
    }.bind(this));

    this.app.get( '/*' , function( req, res, next ) {

        //This is the current file they have requested
        var file = req.params[0];

        //For debugging, we can track what files are requested.
        if(verbose) console.log('Sending %s', path.join(client_dir,file));

        //Send the requesting client the file.
        res.sendFile( '/' + file, {root:path.join(client_dir)} );

    });

    this.sio = this.io.listen(this.server);

    console.log("Listening on port", gameport);

    gserver = new game_server.GameServer(this.Log);

    this.sio.sockets.on('connection', function (client) {
        // Store some state on the client object.
        client.userid = null;
        client.state = 'connected';

        //Useful to know when someone connects
        console.log('socket.io:: player connected');

        client.on('message', function(m) {
            if (client.state === 'connected') {
                var cmd = m[0];
                var data = JSON.parse(m.slice(1));
                if (cmd === 'i') {
                    client.userid = data.id;
                    client.state = 'joined';
                    console.log('socket.io:: player identified as ' + client.userid);
                    gserver.addPlayer(client, data);
                }
            } else {
                gserver.onMessage(client, m);
            }
        });

        client.on('disconnect', function () {
            console.log('socket.io:: player ' + client.userid + ' disconnected');
            gserver.disconnected(client);
        });
    });
}

ExperimentServer.prototype = {};

ExperimentServer.prototype.start = function () {

};

exports.ExperimentServer = ExperimentServer;
