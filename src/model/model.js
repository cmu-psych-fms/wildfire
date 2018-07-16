const args = require('yargs')
      .option('server', {
          describe: "Specify the URL of the server",
          default: "ws://localhost:3000"})
      .option('port', {
          describe: "The port to listen on",
          default: 3000})
      .option('line-ending', {
          describe: "Line endings to use.",
          choices: ['crlf', 'lf'],
          default: 'crlf'})
      .help()
      .argv;

const lineEndingTranslation = {'crlf': '\r\n', 'lf': '\n'};

// var net = require('net');
// var WebSocket = require('ws');

var ModelServer = require('./ModelServer');

ModelServer.startServer(console.log,
                        function (sock, id) {
                            return new ModelServer.ModelInterface(sock, id,
                                                                  {lineEnding: lineEndingTranslation[args['line-ending']]});
                        },
                        {port: args.port});



// var ws = new WebSocket(args.server);

// var c = new Client(1);

// c.connect(ws);

// timer = setInterval(c.update.bind(c), 15);

// var port = args.port;
// var clientNum = 0;
// net.createServer (function (sock) {
//     sock.setEncoding('utf8');
//     sock.setNoDelay(true);

//     clientNum += 1;
//     var client = clientFn(sock, 'model' + clientNum, {lineEnding: opts.lineEnding});
//     client.greet();
//     sock.on ('data', function (data) { client.handleData(data); });
//     sock.on ('error', function (err) {
//         if (err.errno === 'ECONNRESET') { }
//         else { console.log('socket error', err); }
//     });
//     sock.on ('close', function () { client.disconnect(); });
// }).listen (port, defaultHost);
// console.log ('server', 'Listening on port ' + port);

