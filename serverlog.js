const Writable = require('stream').Writable;
const path = require('path');
const fs = require('fs');

function Log(logdir, name) {
    this.logdir = logdir;
    this.name = name;
    this.readonly = false;
}

Log.prototype = {};

Log.prototype.prepareLogDir = function() {
    var readonly = false;
    if (fs.existsSync(this.logdir)) {
        try { fs.accessSync(this.logdir, fs.constants.W_OK); }
        catch (e) { readonly = true; }
    } else {
        try { fs.accessSync(path.dirname(this.logdir), fs.constants.W_OK); }
        catch (e) { readonly = true; }
        if (!this.readonly) {
            try { fs.mkdirSync(this.logdir); }
            catch (e) { if (e.code !== 'EEXIST') throw e; }
        }
    }
    return readonly;
}

Log.prototype.getLogFile = function() {
    var ext = '.log';
    var logfile = path.join(this.logdir, this.name + ext);
    var c = 0;
    // This way of doing it creates a race condition.
    while (fs.existsSync(logfile) && c < 1000) {
        c += 1;
        logfile = path.join(this.logdir, this.name + '.' + c + ext);
    }
    return logfile;
};

Log.prototype.openLogFile = function() {
    this.readonly = this.readonly || this.prepareLogDir();
    if (this.readonly) {
        this.stream = new Writable();
        this.stream._write = () => {};
    } else {
        var f = this.getLogFile();
        this.stream = fs.createWriteStream(f, {flags: 'a'});
    }

    return this.readonly;
};

Log.prototype.startExperiment = function(obj) {
    obj = obj || {};
    this.openLogFile();
    this.startTime = (new Date()).getTime();
    var s = JSON.stringify(obj);
    var comma = s.length > 2 ? ',':'';
    this.stream.write('{' + s.slice(1,s.length-1) + comma +
                         '"body":[\n');
    this.openBody = false;
};

Log.prototype.endExperiment = function(obj) {
    obj = obj || {};
    if (this.openBody) {
        this.stream.write(']}]');
    } else {
        this.stream.write(']');
    }
    var s = JSON.stringify(obj);
    var comma = s.length > 2 ? ',\n':'';
    this.stream.write(comma + s.slice(1,s.length-1) + '}\n');
    this.openBody = false;
    // this.stream.end();
};

Log.prototype.startScreen = function(screen_id, obj) {
    obj = obj || {};
    var time = (new Date()).getTime() - this.startTime;
    var s = JSON.stringify(obj);
    var comma = s.length > 2 ? ',':'';
    this.stream.write((this.openBody ? ']},\n':'')+'{"screen":"'+screen_id+'","ts":'+time + comma + s.slice(1,s.length-1) +
                         ',"body":[\n');
    this.firstBodyElem = true;
    this.openBody = true;
};

Log.prototype.endScreen = function(obj) {
    this.lg('end', obj);
};

Log.prototype.lg = function(tag, obj) {
    obj = obj || {};
    var time = (new Date()).getTime() - this.startTime;
    var s = JSON.stringify(obj);
    var comma = s.length > 2 ? ',':'';
    this.stream.write((this.firstBodyElem ? '':',\n')+'  {"ts":'+time+',"tag":"'+tag+'"'+comma + s.slice(1,s.length-1) + '}');
    this.firstBodyElem = false;
}


exports.Log = Log;
