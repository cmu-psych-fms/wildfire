// Communication with server

var SERVER_URL = '/api';

function Payload (url, timeout, data) {
    this.url = url;
    this.data = data;
    this.timeout = timeout;
    this.completed = false;
    this.timestamp = 0;
    this.request = null;
    this.callback = null;
}

Payload.prototype = {
    send: function () {
        this.timestamp = getCurrentTime();
        this.request = $.ajax(this.url,
                              { 'type' : 'POST',
                                'data' : this.data,
                                'dataType' : 'json',
                                'timeout' : this.timeout
                              });
        this.request.done($.proxy(this.success, this));
        this.request.fail($.proxy(this.fail, this));
    },

    notifyCallback: function (result) {
        if (this.callback) this.callback(result);
    },

    success: function (ret) {
        this.completed = ret.success;
        this.request = null;
        if (this.completed) {
            this.data = null;
        }
        this.notifyCallback(this.completed);
    },

    fail: function (xhr, status, thrownerror) {
        this.request = null;
        this.notifyCallback(false);
    },

    revisit: function () {
        if (this.completed) return;
        if (this.request && this.request.readyState !== 4) {
            if (getCurrentTime() - this.timestamp > this.timeout) {
                this.request.abort();
            } else {
                return;
            }
        }
        this.send();
    },

    abortOverride: function () {
        if (this.completed) return;
        if (this.request) this.request.abort();
        this.request = null;
    }
};

function Com(worker_id, assignment_id, hit_id) {
    this.worker_id = worker_id;
    this.assignment_id = assignment_id;
    this.hit_id = hit_id;
    this.session_id = getCurrentTime();
    this.log_offset = 0;
    this.log_blocks = [];

    this.game_data_requests = [];

    return this;
}

Com.prototype = {};

Com.prototype.isGameLogsComplete = function () {
    var ret = true;
    $.each(this.game_data_requests, function (i, p) { ret = ret && p.completed; });
    return ret;
};

Com.prototype.setGameLogCallback = function (callback) {
    $.each(this.game_data_requests, function (i, p) { p.callback = callback; });
};

Com.prototype.revisitThing = function (thing, override) {
    if (override) {
        $.each(thing, function (i, p) { p.abortOverride(); });
    }
    $.each(thing, function (i, p) { p.revisit(); });
};

Com.prototype.revisitLogBlocks = function (override) {
    this.revisitThing(this.log_blocks, override);
};

Com.prototype.revisitGameData = function (override) {
    this.revisitThing(this.game_data_requests, override);
};

Com.prototype.synchronizeLog = function (log) {
    if (log.length > this.log_offset) {
        var logPortion = log.slice(this.log_offset, log.length);
        this.log_offset = log.length;
        var p = new Payload(SERVER_URL, 10000,
                            { 'action': 'store-log-block',
                              // 'assignment_id' : this.assignment_id,
                              // 'hit_id' : this.hit_id,
                              'worker_id' : this.worker_id,
                              'session_id': this.session_id,
                              'sync_id': this.log_blocks.length,
                              'log': JSON.stringify(logPortion) });
        this.log_blocks.unshift(p);
    }
    this.revisitLogBlocks(false);
};

Com.prototype.sendLog = function (log, extra, successCallback, failCallback) {
    var r;
    r = $.ajax(SERVER_URL,
               { 'type' : 'POST',
                 'data' : { 'action': 'store-log',
                            // 'assignment_id' : this.assignment_id,
                            // 'hit_id' : this.hit_id,
                            'worker_id' : this.worker_id,
                            'extra': JSON.stringify(extra),
                            'log' : JSON.stringify(log) },
                 'dataType' : 'json',
                 'timeout': 60000
               });
    r.done(successCallback);
    r.fail(function (xhr, status, thrownerror) {
        failCallback(status);
    });
};

Com.prototype.storeProgress = function (idx, reward, condition, extra ) {
    var d = { 'action': 'store-progress',
              'worker_id' : getWorkerId(),
              // 'assignment_id' : getAssignmentId(),
              'idx' : idx,
              'reward' : reward,
              'condition' : condition,
              'extra': JSON.stringify(extra),
              'session_id': this.session_id}
    if (isDebugMode()) d.debug = true;
    $.ajax(SERVER_URL,
           { 'type' : 'POST',
             'data' : d,
             'dataType' : 'json' });
};

Com.prototype.sendGameData = function (game_number, logData) {
    var p = new Payload(SERVER_URL, 120000,
                        { 'action': 'store-game-data',
                          'assignment_id' : this.assignment_id,
                          'worker_id' : this.worker_id,
                          'hit_id' : this.hit_id,
                          'session_id': this.session_id,
                          'game' : game_number,
                          'log' : JSON.stringify(logData) });
    this.game_data_requests.unshift(p);
    this.revisitGameData(false);
};

function getResume (resumeCallback, startCallback, rejectCallback, errorCallback) {
    var r = $.ajax(SERVER_URL,
                   { 'type' : 'POST',
                     'data' : { 'action': 'resume',
                                'worker_id' : getWorkerId(),
                                // 'assignment_id' : getAssignmentId(),
                                'condition' : getRawCondition() },
                     'dataType' : 'json' });
    r.done(function (data) {
        if (data.success === true) {
            if (data.reject === true) {
                rejectCallback(data.reason);
            } else if (data.resumable === true) {
                resumeCallback(data);
            } else {
                startCallback(data);
            }
        } else {
            errorCallback('resume failed: ' + JSON.stringify(data));
        }
    });
    r.fail(function (event, xhr, settings, thrownerror) {
        errorCallback('resume totally failed');
    });
}
