var exp;

function getURLParameter(name) {
    var paramstr = window.location.search.substring(1);
    var parampairs = paramstr.split("&");
    for (var i in parampairs) {
        var pair = parampairs[i].split("=");
        if (pair[0] == name) {
                return pair[1];
            }
        }
    return null;
}

function isDebugMode() {
    var id = getURLParameter("debug");
    return id && id == "true";
}

function getAssignmentId() {
    var id = getURLParameter("assignmentId");
    if (!id || id == "ASSIGNMENT_ID_NOT_AVAILABLE")
        return null;
    else
        return id;
}

function getWorkerId() {
    return getURLParameter("workerId");
}

function getHitId() {
    return getURLParameter("hitId");
}

function getRawCondition() {
    return getURLParameter("condition") || "geospatial";
}

function getSubmitTo() {
    var sub = getURLParameter("turkSubmitTo");
    if (sub)
        return decodeURIComponent(sub);
    else
        return null;
}

function getCanvas() {
    return exp.canvas;
}

function getCurrentTime() {
    var d = new Date;
    return d.getTime();
}

function Experiment () {
    this.com = new Com(getWorkerId(), getAssignmentId(), getHitId());

    this.startTime = getCurrentTime();
    this.screens = [];
    this.current = null;
    this.resumep = 0;
    this.condition = '';
    this.reward = 0;
    this.lastBlockReward = 0;
    this.log = [];

    this.gameLogData = [];
    this.gameReward = 0;
    this.gamePoints = 0;

    this.bigHex = 0;
    this.smallHex = 0;

    this.timers = [];

    return this;
}

Experiment.prototype = {};

Experiment.prototype.addReward = function (amount) {
    this.reward += amount;
    this.lastBlockReward += amount;
};

Experiment.prototype.formatDollars = function (amt) {
    var dollars = Math.floor(amt / 100);
    var cents = Math.floor(amt % 100);
    return '$' + dollars + '.' + (cents < 10 ? '0' + cents : cents);
};

Experiment.prototype.formatReward = function () {
    return this.formatDollars(this.reward);
};

Experiment.prototype.resetGameData = function () {
    // zero out arrays without GC
    this.gameLogData.length = 0;
    this.gameReward = 0;
    this.gamePoints = 0;
};

Experiment.prototype.gameLogHeader = function (header) {
    this.gameLogData.push(header);
};

Experiment.prototype.gameLogFooter = function (game_number, footer) {
    this.gameReward = footer.bonus;
    this.gamePoints = footer.points;
    this.gameLogData.push(footer);
    this.com.sendGameData(game_number, this.gameLogData);
};

Experiment.prototype.gameLog = function (state) {
    this.gameLogData.push(state);
};

Experiment.prototype.startTimeout = function (delay, fn) {
    var timer = window.setTimeout(fn, delay);
    this.timers.push(timer);
    return timer;
};

Experiment.prototype.cancelTimeout = function (timer) {
    window.clearTimeout(timer);
    this.timers = this.timers.filter(function (t) { return !(t === timer); });
};

Experiment.prototype.stopAllTimeouts = function () {
    $.each(this.timers, function () { window.clearTimeout(this); });
    this.timers = [];
};

Experiment.prototype.jumpToScreen = function (idx) {
    this.stopAllTimeouts();
    if (this.current !== null) {
        this.screens[this.current].cleanup();
    }
    this.com.synchronizeLog(this.log);
    this.current = Math.min(Math.max(0,idx), this.screens.length-1);
    this.com.storeProgress(this.current, this.reward, this.condition, {gameReward: this.gameReward, gamePoints: this.gamePoints});
    this.screens[this.current].init();
};

Experiment.prototype.nextScreen = function () {
    this.jumpToScreen(this.current + 1);
};

Experiment.prototype.prevScreen = function () {
    this.jumpToScreen(this.current - 1);
};

Experiment.prototype.currentScreen = function () {
    return this.screens[this.current];
};

Experiment.prototype.lg = function (tag, keys, screen_id) {
    var line = keys || {};
    line.time = getCurrentTime() - this.startTime;
    line.screen = screen_id || this.currentScreen().screen_id;
    line.tag = tag;

    // this.storeLine(line);
    this.log.push(line);
};

Experiment.prototype.lgExperimentStart = function (resumep) {
    this.lg("start",
            {"resume": resumep,
             "assignment-id": getAssignmentId(),
             "worker-id": getWorkerId(),
             "hit-id": getHitId(),
             "version": this.version,
             "browser": navigator.userAgent,
             "condition": this.condition},
            "experiment");
};

Experiment.prototype.lgExperimentEnd = function () {
    this.lg("end",
            {"reward": this.reward},
            "experiment");
    this.com.synchronizeLog(this.log);
};

Experiment.prototype.start = function () {
    this.lgExperimentStart(false);
    this.jumpToScreen(0);
};

Experiment.prototype.resume = function (idx) {
    this.resumep = 1;
    this.lgExperimentStart(true);
    this.jumpToScreen(idx);
};

Experiment.prototype.submitHIT = function () {
    $('#experimentform input[name=bonus]').val(this.reward.toString());
    $('#experimentform input[name=condition]').val(this.condition.toString());
    $('#experimentform input[name=resumes]').val(this.resumep.toString());
    $('#experimentform').submit();
};
