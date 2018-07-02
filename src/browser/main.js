function consentBody(include_inputs) {
    var html = '';
    html += '<div class="consent-container">';
    html += '<div id="consent_body" class="consent-text">';
    html += '<h3>Introduction</h3>';
    html += '<p>This task is part of a research study conducted by Dr. John R. Anderson at Carnegie Mellon University.';

    html += '<p>The purpose of this research is to assess the ability to learn a dynamic task.';

    html += '<h3>Procedures</h3>';
    html += '<p>You will be asked to participate.';

    html += '<p>In this study you will be playing a video game. Your goal is to figure out how to play and do as well as possible in 20 games.';

    html += '<p>The expected duration of this study is 80 minutes. That is 60 minutes of gameplay plus time for reading instructions, resting, and answering a few questions.';

    html += '<h3>Participant Requirements</h3>';
    html += '<p>Participation in this study is limited to individuals ages 18 to 40.';

    html += '<h3>Compensation & Costs</h3>';
    html += '<p>You will be compensated for your time on task. You will be compensated $4 for this HIT.';

    html += '<p>You will receive up to $XYZ per game based on your performance.';

    html += ' Because bonuses are based on performance <b>you may earn much less than that</b>. On average, we expect players to make a total of about $XYZ in bonuses.';
    html += '<p>There will be no cost to you if you participate in this study.';

    if (include_inputs) {
        html += '<hr><div style="background: none"><label><input type="checkbox" id="ofage" name="consent" value="ofage" onclick="return checkConsent();">I am between 18 and 40 years old.</label><br/>';
        html += '<label><input type="checkbox" id="understands" name="consent" value="understands" onclick="return checkConsent();">I have read and understand the information above.</label><br/>';
        html += '<label><input type="checkbox" id="participate" name="consent" value="participate" onclick="return checkConsent();">I want to participate in this research and continue with the activity.</label>';
        html += '</div>';
        html += '<div style="background: none; text-align:left; padding: 20px 0 20px 0;"><input type="button" id="consentcontinue" value="Continue" disabled onclick="submitConsent(this.form);"></div>';
    }
    html += '</div>';
    html += '</div>';

    return html;
}

function getNoInstructions(page, version) {
    switch (page) {
    case 0:
        return ('<h3>Instructions</h3>' +
                '<p style="text-align: justify">In this study you will be playing a video game. Your goal is to figure out how to play and do as well as possible in 20 games (1 hour of gameplay).'+
                '<p style="text-align: justify">You play the game using only two keys on your keyboard: <b>W</b> and <b>Space</b>.'+
                '<p style="text-align: justify">It is possible to reach 3000 points in a game by playing perfectly.'+
                '<h3>Bonuses</h3>'+
                '<p style="text-align: justify">After each game, you will receive 10 cents plus 1 cent per 33 points earned in-game.'+
                '<p style="text-align: justify">For example,'+
                '<ul><li>if you earn 500 points in a game you will receive 10+15=25 cents.'+
                '<li>if you average 500 points over 20 games your bonus will be $5.'+
                '<li>if you earn 1000 points in a game you will receive 10+30=40 cents.'+
                '<li>if you average 1000 points over 20 games your bonus will be $8.'+
                '</ul>');
    default:
        return "";
    }
}

function getInstructionsAutoturn(page, version) {
    switch (page) {
    // case 0:
    //     return ('<p style="text-align:center"><video src="video/autoturn.mp4" preload controls></video>');
    case 0:
        return ('<img src="autoturn-snapshot.svg" style="padding: 1.25em 0 0 1em; width:300px" align="right">'+
                '<h3>Instructions</h3>'+

                '<p style="text-align:justify">In this study you will be playing a video game where you control a ship in outer space.'+
                // '<h3>Controls</h3>'+
                // '<p style="text-align:justify">You control the Spaceship with two keys: <b>W</b> and <b>Space Bar</b>.'+
                // '<h3>Navigation</h3>'+
                '<p style="text-align:justify">The ship naturally drifts clockwise and outward. Press the <b>W</b> key to fly the ship inward. The longer you press the <b>W</b> key the faster it will fly inward.'+
                '<p style="text-align:justify">If the ship touches either hexagon it will explode.'+

                '<p style="text-align:justify">The Fortress rotates, following the ship\'s movement. If the ship\'s clockwise motion is too slow, the fortress will fire shells at it.  If your ship is hit, it explodes.'+

                '<p style="text-align:justify">When the ship explodes, you lose 100 points and the spaceship reappears at the start location.'+

    // case 2:
    //     return ('<h3>Destroying the Fortress</h3>'+

                '<p style="text-align:justify">Fire a missile by pressing the <b>Space Bar</b>. Each missile increases the Fortress\' vulnerability by 1. To monitor the vulnerability, see the box labeled VLNER at the bottom of the game screen. When the vulnerability reaches 10, you can destroy the fortress and make 100 points with a "double shot"&mdash;tapping the <b>Space Bar</b> twice, quickly.'+

                '<p style="text-align:justify">If you fire a double shot before the vulnerability reaches 10, it will reset back to 0. '+
                '<p style="text-align:justify">You lose 2 points for each missile fired.'
                // '<p style="text-align:justify">Shortly after the fortress is destroyed it reappears and you can destroy it again for more points.'+
);

    case 1:
        return ('<h3>Summary</h3>'+
                '<p style="text-align:justify">You will be playing 20 games. Each game lasts 3 minutes.'+
                '<p style="text-align:justify">Points are awarded and deducted as follows:'+
                // '<ul><li>Destroy the fortress: +100 points'+
                // '<li>Ship explosion: -100 points'+
                // '<li>Firing a missile: -2 points'+
                // '</ul>'+

                '<table style="display:inline-block; border-spacing: 2px;">'+
                '<tr><td class="score">Destroy the fortress<td class="score">+100 points'+
                '<tr><td class="score">Ship Explosion<td class="score">-100 points'+
                '<tr><td class="score">Fire a Missile<td class="score"> -2 points'+
                '</table>'+

                '<p>It is possible to reach 3000 points in a game by playing perfectly.'+
                '<h3>Bonus Money</h3>'+
                '<p style="text-align: justify">After each game, you will receive 1 cent per 50 points earned.'+
                '<p style="text-align: justify">For example,'+
                '<ul><li>if you earn 500 points in a game you will receive 10 cents.'+
                '<li>if you average 500 points over 20 games your bonus will be $2.'+
                '<li>if you earn 1000 points in a game you will receive 20 cents.'+
                '<li>if you average 1000 points over 20 games your bonus will be $4.');
    default:
        return '';
    }
}

function getInstructionsNoSemanticsAutoturn(page) {
    switch (page) {
    // case 0:
    //     return ('<p style="text-align:center"><video src="video/nosemantics.mp4" preload controls></video>');
    case 0:
        return ('<img src="nosemantics-snapshot.svg" style="padding: 1.25em 0 0 1em; width:300px" align="right">'+
                '<h3>Instructions</h3>'+

                '<p style="text-align:justify">In this study you will be playing a video game where you control a yellow ball.'+
                // '<h3>Controls</h3>'+
                // '<p style="text-align:justify">You control the Spaceship with two keys: <b>W</b> and <b>Space Bar</b>.'+
                // '<h3>Navigation</h3>'+
                '<p style="text-align:justify">The ball naturally drifts clockwise and outward. Press the <b>W</b> key to move the ball inward. The longer you press the <b>W</b> key the faster it will move inward.'+
                '<p style="text-align:justify">If the ball touches either hexagon it will reappear at the start location.'+

                '<p style="text-align:justify">If the ball\'s clockwise motion is too slow, The hexagons will pulse bright green and the ball may suddenly reappear at the start location.'+
                // '<p style="text-align:justify">If the ball\'s clockwise motion is too slow, it may suddenly reappear at the start location. The hexagons will pulse bright green to warn of this possibility.'

                '<p style="text-align:justify">When the ball reappears at the start location, you lose 100 points.'+

    // case 2:
    //     return ('<h3>Destroying the Fortress</h3>'+

                '<p style="text-align:justify">Tap the <b>Space Bar</b> to increment the counter by 1. To monitor the counter, see the box labeled COUNTER at the bottom of the game screen. When the counter reaches 10, you can make 100 points with a "double tap"&mdash;tapping the <b>Space Bar</b> twice, quickly.'+

                '<p style="text-align:justify">If you do a double tap before the counter reaches 10, it will reset back to 0. '+
                '<p style="text-align:justify">You lose 2 points for each tap of the <b>Space Bar</b>.'
                // '<p style="text-align:justify">Shortly after the fortress is destroyed it reappears and you can destroy it again for more points.'+
);

    case 1:
        return ('<h3>Summary</h3>'+
                '<p style="text-align:justify">You will be playing 20 games. Each game lasts 3 minutes.'+
                '<p style="text-align:justify">Points are awarded and deducted as follows:'+
                // '<ul><li>Destroy the fortress: +100 points'+
                // '<li>Ship explosion: -100 points'+
                // '<li>Firing a missile: -2 points'+
                // '</ul>'+

                '<table style="display:inline-block; border-spacing: 2px;">'+
                '<tr><td class="score">Double Tap (when counter &ge; 10)<td class="score">+100 points'+
                '<tr><td class="score">Ball Restart<td class="score">-100 points'+
                '<tr><td class="score">Tap the Space Bar<td class="score"> -2 points'+
                '</table>'+

                '<p>It is possible to reach 3000 points in a game by playing perfectly.'+
                '<h3>Bonus Money</h3>'+
                '<p style="text-align: justify">After each game, you will receive 1 cent per 50 points earned.'+
                '<p style="text-align: justify">For example,'+
                '<ul><li>if you earn 500 points in a game you will receive 10 cents.'+
                '<li>if you average 500 points over 20 games your bonus will be $2.'+
                '<li>if you earn 1000 points in a game you will receive 20 cents.'+
                '<li>if you average 1000 points over 20 games your bonus will be $4.');
    default:
        return '';
    }
}

function createScreens(condition) {
    var screens = [];
    var numGames = 20;
    var instructions, pages;

    instructions = getNoInstructions;
    pages = 1;

    var instructionText = ('<p>Play the game with these keys:'+
                           '<div style="text-align: center; padding: 10px 0 0 0;">'+
                           '<div class="instr"><span class="keyblock">W</span><br><span class="keyinstr">Thrust</span></div>'+
                           '<div class="instr"><span class="spaceblock">Space</span><br><span class="keyinstr">Fire Missile</span></div></div>'+
                           '<p>Games are 3 minutes long.'+
                           '<p>To review the instructions, click <a id="instructionslink" href="#">here</a>.'+
                           '<p>When you are ready to start the game, press ENTER.');

    var i;

    // screens.push(new Consent(consentBody(true, payment)));
    // screens.push(new DemographicSurvey());
    // screens.push(new SoundCheck(['fire-missile', 'fire-shell', 'vlner-reset']));

    // screens.push(new Instructions(instructions, pages));

    for (i=0; i<numGames; i++) {
        var g = new WebClient(i+1);
        var s = new ScoreScreen(i+1, 10);
        var n = i+1;
        // screens.push(new GameStartScreen('<h1>Game '+n.toString()+' of '+numGames.toString()+'</h1>' + instructionText, instructions, pages));
        screens.push(g);
        screens.push(s);
    }

    screens.push(new MessageScreen('<h1>Thanks!</h1>' +
                                   '<p>We just have a few follow-up questions for you about the games you played.'));
    // screens.push(new GameRules());
    // screens.push(new GameQuestions();
    // screens.push(new Feedback());

    screens.push(new End('<h1>The End</h1>'+
                         '<p>Thanks for participating!'));

    return screens;
}

function showNoAudio() {
    $('#experiment_area').html('<div class="message-container"><div class="message-body">'+
                               '<h2>Your browser does not support audio playback</h2>'+
                               '<p>This study uses sound effects for in-game events.<p>Please load this HIT in a different browser such as the latest version of: Chrome, Firefox, or Safari.'+
                              '</div></div>');
}

function showPreview() {
    $('#experiment_area').html(consentBody(false));
}

function showImagesLoadingScreen() {
    $('#experiment_area').html('<div class="loading">' +
                               '<div class="loadingline">Loading Images...</div>' +
                               '</div>');
}

function showLoadingScreen() {
    $('#experiment_area').html('<div class="loading">' +
                               '<div class="loadingline">Loading...Please Wait.</div>' +
                               '</div>');
}

function showRejectScreen(body) {
    $('#experiment_area').html('<div class="reject-container">' +
                               '<div class="reject-body">' +
                               body +
                               '</div>' +
                               '</div>');
}

function resumeErrorCallback (text) {
    $('#experiment_area').html('<div class="survey">' +
                               '<div class="question">Oops! Something went wrong. Reloading the page may help. If it doesn\'t please report this problem.</div>' +
                               '<div class="error" id="errorPageError">'+text+'</div>' +
                               '</div>');
}

function rejectExperimentCallback (reason) {
    if (reason === 'assignment-mismatch') {
        showRejectScreen('<h3>Please return the HIT</h3><p>Our records show that you have already participated in this study.');
    } else {
        showRejectScreen('<h3>Please return the HIT</h3><p>Our records show that you have already participated in a similar study.');
    }
}

function resumeExperimentCallback (data) {
    exp = new Experiment();
    exp.condition = getRawCondition(); // data.condition;
    exp.screens = createScreens(exp.condition);
    exp.reward = data.reward;
    exp.gameReward = data.extra.gameReward;
    exp.gamePoints = data.extra.gamePoints;
    exp.bigHex = data.bighex;
    exp.smallHex = data.smallhex;
    exp.resume(data.idx);
}

function startExperimentCallback (data) {
    exp = new Experiment();
    exp.condition = data.condition;
    exp.screens = createScreens(exp.condition);
    exp.start();
}

var g_sounds = {};

function load_audio() {
    g_sounds[1] = new Howl({src: ['sound/cometome.wav',
                                'sound/cometome.mp3']});
    g_sounds[3] = new Howl({src: ['sound/followme.wav',
                                'sound/followme.mp3']});
    g_sounds[4] = new Howl({src: ['sound/intransit.wav',
                                'sound/intransit.mp3']});
    g_sounds[2] = new Howl({src: ['sound/splitup.wav',
                                'sound/splitup.mp3']});
}

var g_images = {};

var g_jq;

var g_images_are_loaded = false;

function load_image(name) {
    g_images[name] = new Image();
    g_images[name].src = "sprites/"+name+".png";
    g_jq.push(g_images[name]);
}

function load_images(callback) {
    g_jq = [];

    load_image('bird');
    load_image('ceiling');
    load_image('hills');
    load_image('tube-top');
    load_image('tube-middle');
    load_image('egg');
    load_image('hatchedegg');
    load_image('nest');
    load_image('bricks');
    load_image('darkbricks');
    load_image('scoreboard');
    load_image('cloud');
    load_image('pedestal');
    load_image('topclouds');
    load_image('lightning');
    load_image('scorchedbird');
    load_image('deadbird');

    g_images['bg'] = new Image();
    g_images['bg'].src = "sprites/background.png";
    g_jq.push(g_images['bg']);
    imagesLoaded(g_jq, {}, callback);
}

function main () {
    if (getAssignmentId() && getHitId() && getWorkerId()) {
        disable_backspace();
        load_audio();
        fillInForm();
        if (Howler.noAudio) {
            showNoAudio();
        } else {
            showLoadingScreen();
            getResume(resumeExperimentCallback, startExperimentCallback, rejectExperimentCallback, resumeErrorCallback);
            if (isDebugMode()) {
                // $(document).on('keydown', skipAround);
            }
        }
    } else {
        showPreview();
    }
}
