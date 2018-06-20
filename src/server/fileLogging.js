var uuid = require('uuid/v1');
var fs = require('fs');
var path = require('path');

function FileLogging(datadir) {
    this.datadir = datadir;
    console.log('logging to', this.datadir);
    // this.sessionId = uuid();
    this.sessionId = 'test'
    this.sessionDir = path.join(this.datadir, this.sessionId);

    this.workerStreams = {};
}

FileLogging.prototype = {};

FileLogging.prototype.setup = function () {
    if (fs.existsSync(this.sessionDir)) {
        console.log("whaaaat?");
    } else {
        fs.mkdirSync(this.sessionDir);
    }
};

FileLogging.prototype.openDB = function () {
    this.setup();
};

FileLogging.prototype.updateProgress = function (worker_id, idx, reward, condition, extra, session_id) {
    // this.db.get('SELECT worker_id, idx from resume where worker_id = ?', [worker_id],
    //             function (err, row) {
    //                 if (row) {
    //                     this.db.run('UPDATE resume SET idx = ?, reward = ?, exp_condition = ?, extra = ? WHERE worker_id = ?', [idx, reward, condition, extra, worker_id]);
    //                 } else {
    //                     this.db.run('INSERT INTO resume (worker_id,assignment_id,idx,reward,exp_condition,extra) VALUES (?,?,?,?,?,?)',
    //                                 [worker_id,
    //                                  'lab',
    //                                  idx,
    //                                  reward,
    //                                  condition,
    //                                  extra]);
    //                 }
    //             }.bind(this));
    // this.db.run('INSERT INTO progress_log (worker_id,assignment_id,idx,reward,exp_condition,extra,session_id) VALUES (?,?,?,?,?,?,?)',
    //             [worker_id, 'lab', idx, reward, condition, extra, session_id]);
};

FileLogging.prototype.addGameLog = function (worker_id, glog, session_id, gnum) {
    // var out = fs.createWriteStream(path.join(this.sessionDir,worker_id + '-' + gnum + '.log'

    // this.db.run('INSERT INTO game_data (worker_id, session_id, game_number, log)'+
    //             'VALUES (?,?,?,?)',
    //             [worker_id, session_id, gnum, log]);
};

FileLogging.prototype.addLogBlock = function (worker_id, lines, session_id, sync_id) {
    if (!this.workerStreams[worker_id]) {
        this.workerStreams[worker_id] = fs.createWriteStream(path.join(this.sessionDir,worker_id + '.log'), {flags: 'a'});
    }
    // this.workerStreams[worker_id].write(lines);
    // this.workerStreams[worker_id].write('\n');
    lines = JSON.parse(lines);
    for (let i=0; i<lines.length; i++) {
        this.workerStreams[worker_id].write(JSON.stringify(lines[i]) + '\n');
    }
};

FileLogging.prototype.addSessionLog = function (worker_id, log) {
};

FileLogging.prototype.getResume = function (worker_id, callback) {
    // File logging doesn't support resume
    callback({success:true, resumable:false});
};

// Server-side logging

FileLogging.prototype.startGame = function (game_number, extra) {
    if (this.gameStream) this.gameStream.end();
    this.gameStream = fs.createWriteStream(path.join(this.sessionDir, 'server-' + game_number.toString() + '.log'));
    this.gameStream.write('[');
    this.gameStream.write(JSON.stringify(extra));
}

FileLogging.prototype.endGame = function (game_number, extra) {
    this.gameStream.write(',\n' + JSON.stringify(extra));
    this.gameStream.write(']');
    this.gameStream.end();
    this.gameStream = null;
};

FileLogging.prototype.saveGameState = function (game_number, tick, state) {
    this.gameStream.write(",\n" + JSON.stringify(state));
};

exports.FileLogging = FileLogging;
