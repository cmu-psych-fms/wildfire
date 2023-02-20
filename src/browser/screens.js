/**
 * @license
 * Copyright 2018,2019 Shawn Betts
 * SPDX-License-Identifier: MIT
**/

function ScoreScreen (game_number, conversion) {
    this.screen_id = 'score';
    this.game_number = game_number;
    this.conversion = conversion;
    return this;
}

ScoreScreen.prototype = {};

ScoreScreen.prototype.init = function () {
    $('#experiment_area').html('<div class="message-container"><div class="message-body">'+
                               '<h1 style="text-align: center;">Score</h1>' +
                               '<div style="text-align: left; display: inline-block;">'+
                               '<p>You scored '+exp.gamePoints.toString()+' points.' +
                               '<p>At 1 cent per '+this.conversion+' points, you earned a bonus of '+exp.formatDollars(exp.gameReward)+' this game.'+
                               '<p>In total you have earned a bonus of '+exp.formatReward()+'.'+
                               '<p style="text-align: center"><input type="button" value="Continue" id="game_data_continue">'+
                               '</div></div></div>');
    exp.lg('start', {'n': this.game_number});
    $(document).on('keypress', $.proxy(this.keypress, this));
    $('#game_data_continue').on('click', $.proxy(this.onward, this));
};

ScoreScreen.prototype.clearEvents = function (ev) {
    $(document).off('keypress', this.keypress);
};

ScoreScreen.prototype.keypress = function (ev) {
    // Gobble thrust and fire
    if (ev.which === 32 || ev.which === 119) {
        // console.log('gobble');
        ev.preventDefault();
    }
};

ScoreScreen.prototype.onward = function () {
    if (!isFullScreen()) {
        fullScreenMode();
    } else {
        exp.nextScreen();
    }
};

ScoreScreen.prototype.cleanup = function () {
    this.clearEvents();
    exp.resetGameData();
    exp.lg('end');
};

function InstructionsGuts(pagefn, numPages, finishHook) {
    this.timeout = 1000;
    this.curPage = 0;
    this.getPage = pagefn;
    this.numPages = numPages;
    this.finishHook = finishHook;
    return this;
}

InstructionsGuts.prototype = {
    init: function () {
        $('#experiment_area').html('<div class="full-message-body">'+
                                   '<div id="full-message-text" class="message-text">'+
                                   '</div>'+
                                   '<div class="footer-continue-area"><div id="pagenum" class="pagenum"></div><input class="footer-continue" type="button" id="prev-button" value="Previous Page"><input class="footer-continue" type="button" id="next-button" value="Next Page"></div>'+
                                   '</div>');

        $('#prev-button').on('click', $.proxy(this.prevPage, this));
        $('#next-button').on('click', $.proxy(this.nextPage, this));

        if (this.numPages === 1) {
            $('#prev-button').hide();
        }

        exp.lg('start-instructions');
    },

    enableButtons: function () {
        if (this.curPage > 0) {
            $('#prev-button').prop('disabled', false);
        }
        $('#next-button').prop('disabled', false);
    },

    prevPage: function () {
        this.jumpToPage(this.curPage-1);
    },

    nextPage: function () {
        if (this.curPage == this.numPages-1) {
            exp.lg('finish-instructions');
            this.finishHook();
        } else {
            this.jumpToPage(this.curPage+1);
        }
    },

    jumpToPage: function (n) {
        if (n >= 0 && n < this.numPages) {
            this.curPage = n;
            if (this.curPage == this.numPages-1) {
                $('#next-button').val('Continue');
            } else {
                $('#next-button').val('Next Page');
            }
            $('#full-message-text').html(this.getPage(n));

            if (this.numPages > 1) {
                var pg = this.curPage + 1;
                $('#pagenum').html('Page '+pg.toString()+' of '+this.numPages.toString());
            }
            
            $('#prev-button').prop('disabled', true);
            $('#next-button').prop('disabled', true);
            exp.startTimeout(this.timeout, $.proxy(this.enableButtons, this));

            exp.lg('page', {'n': n});
        }
    }
};

function Instructions(pagefn, numPages) {
    this.screen_id = 'Instructions';
    this.getPage = pagefn;
    this.numPages = numPages;

    return this;
}

Instructions.prototype = {};

Instructions.prototype.init = function () {
    this.guts = new InstructionsGuts(this.getPage, this.numPages, $.proxy(this.finish, this));
    exp.lg('start');
    this.guts.init();
    this.guts.jumpToPage(0);
};

Instructions.prototype.finish = function () {
    exp.nextScreen();
};

Instructions.prototype.cleanup = function () {
    exp.lg('end');
};

function GameStartScreen(text, instructionsfn, numPages) {
    this.screen_id = 'gamestart';
    this.text = text;
    this.getInstructions = instructionsfn;
    this.numPages = numPages;

    return this;
}

GameStartScreen.prototype = {};

GameStartScreen.prototype.init = function () {
    this.showStart();
    this.guts = new InstructionsGuts(this.getInstructions, this.numPages, $.proxy(this.finishInstructions, this));
    exp.lg('start', {'text': this.text});
};

GameStartScreen.prototype.showStart = function () {
    $('#experiment_area').html('<div class="message-container"><div class="message-body">'+
                               this.text +
                               '</div></div>');
    $('#instructionslink').click('on', $.proxy(this.showInstructions, this));
    $(document).on('keypress', $.proxy(this.keypress, this));
    this.continueEnabled = false;
    exp.startTimeout(500, $.proxy(this.allowContinue, this));
};

GameStartScreen.prototype.showInstructions = function () {
    $(document).off('keypress', this.keypress);
    exp.stopAllTimeouts();
    this.guts.init();
    this.guts.jumpToPage(0);
};

GameStartScreen.prototype.finishInstructions = function () {
    this.showStart();
};

GameStartScreen.prototype.allowContinue = function (ev) {
    this.continueEnabled = true;
};

GameStartScreen.prototype.keypress = function (ev) {
    if (ev.which === 13) {
        ev.preventDefault();
        if (!isFullScreen()) {
            fullScreenMode();
        } else {
            if (this.continueEnabled) exp.nextScreen();
        }
    }
};

GameStartScreen.prototype.clickContinue = function (ev) {
    exp.nextScreen();
};

GameStartScreen.prototype.cleanup = function () {
    $(document).off('keypress', this.keypress);
    exp.lg('end');
};


function MessageScreen(text) {
    this.screen_id = 'message';
    this.text = text;

    return this;
}

MessageScreen.prototype = {};

MessageScreen.prototype.init = function () {
    $('#experiment_area').html('<div class="message-container"><div class="message-body">'+
                               this.text +
                               '<p><input class="footer-continue" type="button" id="continue_button" value="Continue">'+
                               '</div></div>');
    // $(document).on('keypress', $.proxy(this.keypress, this));
    $('#continue_button').prop('disabled', true);
    $('#continue_button').on('click', $.proxy(this.clickContinue, this));
    this.continueEnabled = false;
    exp.startTimeout(1000, $.proxy(this.allowContinue, this));
    exp.lg('start', {'text': this.text});
};

MessageScreen.prototype.allowContinue = function (ev) {
    $('#continue_button').prop('disabled', false);
    // this.continueEnabled = true;
};

MessageScreen.prototype.keypress = function (ev) {
    if (this.continueEnabled && ev.which === 13) {
        ev.preventDefault();
        exp.nextScreen();
    }
};

MessageScreen.prototype.clickContinue = function (ev) {
    if (!isFullScreen()) {
        fullScreenMode();
    } else {
        exp.nextScreen();
    }
};

MessageScreen.prototype.cleanup = function () {
    // $(document).off('keypress', this.keypress);
    exp.lg('end');
};

function FullMessageScreen(msg) {
    this.screen_id = 'message';
    this.msg = msg;
    this.timeout = 1000;
    return this;
}

FullMessageScreen.prototype = {};

FullMessageScreen.prototype.init = function () {
    var m = $('#experiment_area').html('<div class="full-message-body">'+
                                       '<div id="full-message-text" class="message-text">'+
                                       this.msg +
                                       '</div>'+
                                       '<div class="footer-continue-area"><input class="footer-continue" type="button" id="full-message-button" value="Continue"></div>'+
                                       '</div>');
    $('#full-message-button')
        .prop('disabled', true)
        .on('click', $.proxy(this.click, this));
    var trial = this;
    exp.startTimeout(this.timeout, function () { trial.enableContinue(); });
    exp.lg('start', {'text': m.html()});
};

FullMessageScreen.prototype.enableContinue = function () {
    $('#full-message-button').prop('disabled', false);
};

FullMessageScreen.prototype.click = function () {
    exp.nextScreen();
};

FullMessageScreen.prototype.cleanup = function () {
    exp.lg('end');
};


function Consent(body) {
    this.screen_id = 'consent';
    this.body = body;
    return this;
}

Consent.prototype = {}; //Object.create(Screen.prototype);

Consent.prototype.init = function () {
    $('#experiment_area').html(this.body);
    exp.lg('start');
};

Consent.prototype.onward = function () {
    exp.lg('end');
    exp.nextScreen();
};

Consent.prototype.cleanup = function () {
};

function checkConsent() {
    var a = document.getElementById('ofage');
    var u = document.getElementById('understands');
    var p = document.getElementById('participate');
    var c = document.getElementById('consentcontinue');
    if (a.checked && u.checked && p.checked) {
        c.disabled = false;
    } else {
        c.disabled = true;
    }
}

function submitConsent() {
    exp.currentScreen().onward();
}

function End(body) {
    this.screen_id = 'end';
    this.failReasons = [];
    this.body = body;
    return this;
}
End.prototype = {
    retryButtonDuration: 10000,
    sendDuration: 120000,
    init: function () {
        $('#experiment_area').html('<div class="message-container">'+
                                   '<div class="message-body">'+
                                   '<div id="end-text" class="end-text">'+
                                   this.body +
                                   '<p>Your bonus is <span class="bonus"></span>.'+
                                   '<div id="send_data" style="display: inline-block; height: 100px"></div>'+
                                   // '<p><input class="continue" type="button" id="end_button" value="Submit Results" onclick="endClick();">'+
                                   '</div>'+
                                   '</div>'+
                                   '</div>');
        $('span.bonus').text(exp.formatReward());
        this.overrideGameData = false;
        this.attemptCount = 0;
        this.logDataComplete = false;
        exp.lgExperimentEnd();
        this.sendData();
    },

    // createDataMatrix: function () {
    //     var ret = '<div id="progress" style="width: 200px; height: 50px; border: 1px solid black>';
    //     var sum = 0;
    //     var max = exp.com.game_data_requests.length + 1;
    //     var i;

    //     for (i=0; i<exp.com.game_data_requests; i++) {
    //         if (exp.com.game_data_requests[i].completed) {
    //             sum += 1;
    //         }
    //     }
    //     var p = sum/max * 100;

    //     ret += '<div id="percentdone" style="width: '+p.toString()+'%; height:100%; background: #0000FF">';
    //     ret += '</div></div>';
    //     return ret;
    // },

    sendData: function (ev) {
        var firstTime = "Sending HIT data to server...";
        var nthTime = 'Resending HIT data&mdash;attempt '+this.attemptCount.toString();
        var msg = this.attemptCount > 0 ? nthTime:firstTime;

        $('#send_data').html('<div class="sending sendprogress">'+msg+'<br>Do NOT reload this page!</div><p style="text-area: center"><img src="spinner.svg">'+
                             '<p><input type="button" value="Retry" id="retry_button">');

        exp.stopAllTimeouts();
        exp.startTimeout(this.retryButtonDuration, $.proxy(this.enableRetryButton, this));
        exp.startTimeout(this.sendDuration, $.proxy(this.sendTimeout, this));

        if (!this.logDataComplete) {
            exp.com.sendLog(exp.log, this.failReasons, $.proxy(this.logDataSuccess, this), $.proxy(this.logDataFail, this));
        }
        exp.com.setGameLogCallback($.proxy(this.gameLogCallback, this));
        exp.com.revisitGameData(this.overrideGameData);

        $('#retry_button')
            .css('visibility', 'hidden')
            .on('click', $.proxy(this.clickRetry, this));
    },

    showFail: function () {
        $('#send_data').html('<div class="sending sendfail">Failed to send HIT data.<br>Please click the button to try again.</div>' +
                             '<p style="text-align: center"><input type="button" value="Try Again" id="game_data_retry">');
        $('#game_data_retry').on('click', $.proxy(this.sendData, this));
        exp.stopAllTimeouts();
        this.attemptCount += 1;
    },

    maybeShowSubmitButton: function () {
        if (exp.com.isGameLogsComplete() && this.logDataComplete) {
            $('#send_data').html('<div class="sending sendsuccess">HIT data successfully sent to server.<br>Please click the button to submit the HIT.'+
                                 '</div><p style="text-align: center"><input type="button" value="Submit HIT" id="end_button">');
            $('#end_button').on('click', $.proxy(this.submit, this));
            exp.stopAllTimeouts();
        }
    },

    enableRetryButton: function () {
        $('#retry_button').css('visibility', 'visible');
    },

    clickRetry: function () {
        this.failReasons.push('retry');
        this.retry('retry');
    },

    sendTimeout: function () {
        this.failReasons.push('timeout');
        this.showFail();
    },

    retry: function () {
        this.overrideGameData = true;
        this.attemptCount += 1;
        this.sendData();
    },

    gameLogCallback: function (result) {
        if (result) {
            this.maybeShowSubmitButton();
        }
    },

    logDataSuccess: function (data) {
        if (data.success) {
            this.logDataComplete = true;
            this.maybeShowSubmitButton();
        } else {
            this.logDataFail(data.reason);
        }
    },

    logDataFail: function (status) {
        this.failReasons.push(status?status:"unknown");
        console.log('log fail', status);
    },

    submit: function () {
        exp.submitHIT();
    },

    cleanup: function () {
    }
};

function GetConditionScreen () {
    return this;
}

GetConditionScreen.prototype = {};
