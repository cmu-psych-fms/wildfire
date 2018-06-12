var fs = require('fs');
var path = require('path');
var sqlite3 = require('sqlite3').verbose();

function Logging(datadir) {
    this.dbfile = path.join(datadir, 'logdb.sqlite');
}

Logging.prototype = {};

Logging.prototype.openDB = function () {
    if (fs.existsSync(this.dbfile)) {
        this.db = new sqlite3.Database(this.dbfile, sqlite3.OPEN_READWRITE);
    } else {
        this.db = new sqlite3.Database(this.dbfile, sqlite3.OPEN_READWRITE | sqlite3.OPEN_CREATE);
        this.createDB();
    }
};

Logging.prototype.createDB = function () {
    this.db.run('CREATE TABLE log_data ('+
                'id integer PRIMARY KEY AUTOINCREMENT,'+
                'created_on timestamp DEFAULT CURRENT_TIMESTAMP,'+
                'worker_id varchar(50) NOT NULL,'+
                'assignment_id varchar(50) NOT NULL,'+
                'hit_id varchar(50) NOT NULL,'+
                'remote_ip varchar(20),'+
                'extra mediumtext,'+
                'log mediumtext);');

    this.db.run('CREATE TABLE game_data ('+
                'id integer PRIMARY KEY AUTOINCREMENT,'+
                'created_on timestamp DEFAULT CURRENT_TIMESTAMP,'+
                'worker_id varchar(50) NOT NULL,'+
                'assignment_id varchar(50) NOT NULL,'+
                'hit_id varchar(50) NOT NULL,'+
                'remote_ip varchar(20),'+
                'session_id bigint NOT NULL,'+
                'game_number int,'+
                'log mediumtext'+
                ');');

    this.db.run('CREATE TABLE log_block ('+
                'id integer PRIMARY KEY AUTOINCREMENT,'+
                'created_on timestamp DEFAULT CURRENT_TIMESTAMP,'+
                'worker_id varchar(50) NOT NULL,'+
                'assignment_id varchar(50) NOT NULL,'+
                'hit_id varchar(50) NOT NULL,'+
                'remote_ip varchar(20),'+
                'session_id bigint NOT NULL,'+
                'sync_id int,'+
                'log mediumtext'+
                ');');

    this.db.run('CREATE TABLE resume ('+
                'worker_id varchar(50) NOT NULL,'+
                'idx varchar(50),'+
                'reward varchar(50),'+
                'exp_condition varchar(50),'+
                'extra mediumtext,'+
                'assignment_id varchar(50) NOT NULL,'+
                'note varchar(50)'+
                ');');

    // Every time the resume table is updated, a line in this table is
    // added to record that change.
    this.db.run('CREATE TABLE progress_log ('+
                'worker_id varchar(50) NOT NULL,'+
                'assignment_id varchar(50) NOT NULL,'+
                'created_on timestamp DEFAULT CURRENT_TIMESTAMP,'+
                'remote_ip varchar(20),'+
                'idx varchar(50),'+
                'session_id bigint NOT NULL,'+
                'reward varchar(50),'+
                'exp_condition varchar(50),'+
                'extra mediumtext'+
                ');');

    // Every time a resume is requested, it gets logged.
    this.db.run('CREATE TABLE resume_log ('+
                'worker_id varchar(50) NOT NULL,'+
                'assignment_id varchar(50) NOT NULL,'+
                'created_on timestamp DEFAULT CURRENT_TIMESTAMP,'+
                'remote_ip varchar(20),'+
                'idx varchar(50),'+
                'reward varchar(50),'+
                'exp_condition varchar(50),'+
                'extra mediumtext'+
                ');');

};

Logging.prototype.updateProgress = function (worker_id, idx, reward, condition, extra, session_id) {
    this.db.get('SELECT worker_id, idx from resume where worker_id = ?', [worker_id],
                function (err, row) {
                    if (row) {
                        this.db.run('UPDATE resume SET idx = ?, reward = ?, exp_condition = ?, extra = ? WHERE worker_id = ?', [idx, reward, condition, extra, worker_id]);
                    } else {
                        this.db.run('INSERT INTO resume (worker_id,assignment_id,idx,reward,exp_condition,extra) VALUES (?,?,?,?,?,?)',
                                    [worker_id,
                                     'lab',
                                     idx,
                                     reward,
                                     condition,
                                     extra]);
                    }
                }.bind(this));
    this.db.run('INSERT INTO progress_log (worker_id,assignment_id,idx,reward,exp_condition,extra,session_id) VALUES (?,?,?,?,?,?,?)',
                [worker_id, 'lab', idx, reward, condition, extra, session_id]);
};

Logging.prototype.addGameLog = function (worker_id, glog, session_id, gnum) {
    this.db.run('INSERT INTO game_data (worker_id, session_id, game_number, log)'+
                'VALUES (?,?,?,?)',
                [worker_id, session_id, gnum, log]);
};

Logging.prototype.addLogBlock = function (worker_id, blk, session_id, sync_id) {
    this.db.run('INSERT INTO log_block (worker_id, assignment_id, hit_id, session_id, sync_id, log)'+
                'VALUES (?,?,?,?,?,?)',
                [worker_id, 'lab', 'lab', session_id, sync_id, blk]);
};

Logging.prototype.addSessionLog = function (worker_id, log) {
    this.db.run('INSERT INTO log_data (worker_id, log)'+
                'VALUES (?,?)',
                [worker_id, log]);
};

Logging.prototype.getResume = function (worker_id, callback) {
    this.db.get("SELECT * from resume where worker_id=?", [worker_id],
                function (err, row) {
                    if (row) {
                        callback({success:true, resumable:true,
                                  idx: row.idx, reward: row.reward,
                                  condition: row.condition, extra: JSON.parse(row.extra)});
                    } else {
                        callback({success:true, resumable:false});
                    }
                });
};

exports.Logging = Logging;
