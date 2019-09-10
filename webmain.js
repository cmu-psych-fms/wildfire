var game = {};

var g_images = {};
var g_jq = [];

function load_image(name) {
    g_images[name] = new Image();
    g_images[name].src = "sprites/"+name+".png";
    g_jq.push(g_images[name]);
}

function start_player_client () {
    imagesLoaded(g_jq, {}, function () {
        game = new WebClient('player');
        game.begin();
        show_menu();
        hide_join_buttons();
    });
}

function start_observer_client () {
    imagesLoaded(g_jq, {}, function () {
        game = new WebClient('observer');
        game.begin();
        show_menu();
        hide_join_buttons();
    });
}

function reset_game () {
    if (game) game.sendReset();
};

function show_instructions () {
    var elem = document.getElementById('instructions_area');
    elem.style.display = 'block';
};

function hide_instructions () {
    document.getElementById('instructions_area').style.display = 'none';
};

function hide_menu () {
    document.getElementById('menu').style.display = 'none';
};

function show_menu () {
    document.getElementById('menu').style.display = 'block';
};

function hide_join_buttons () {
    document.getElementById('join').style.display = 'none';
};

function fullscreen () {
    // var elem = document.getElementById('gamearea');
    var elem = document.body;
    if (elem.requestFullscreen) {
        elem.requestFullscreen();
    } else if (elem.mozRequestFullScreen) { /* Firefox */
        elem.mozRequestFullScreen();
    } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
        elem.webkitRequestFullscreen();
    } else if (elem.msRequestFullscreen) { /* IE/Edge */
        elem.msRequestFullscreen();
    }
    game.resizeCanvas();
};

window.onload = function(){
    load_image('fire');
    load_image('ash');
    load_image('retardant');
    load_image('water');
    load_image('tree');
    load_image('house');
    load_image('rock');
    load_image('hroad');
    load_image('vroad');
    load_image('plane');
    load_image('grass');
    load_image('smoke');
    load_image('watertankfull');
    load_image('watertankempty');
    load_image('flame');
    load_image('flame2');

    hide_instructions();
    hide_menu();
};
