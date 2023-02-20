/**
 * @license
 * Copyright 2018,2019 Shawn Betts
 * SPDX-License-Identifier: MIT
**/

var g_game = false, g_lobby;

var g_images = {};
var g_jq = [];

function load_image(name) {
    g_images[name] = new Image();
    g_images[name].src = "sprites/"+name+".png";
    g_jq.push(g_images[name]);
}

function show_instructions () {
    var elem = document.getElementById('instructions_area');
    elem.style.display = 'block';
};

function hide_instructions () {
    var elem = document.getElementById('instructions_area');
    elem.style.display = 'none';
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
    // game.resizeCanvas();
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

    document.getElementById('instructions_area').style.display = 'none';
    document.getElementById('gamearea').style.display = 'none';
    document.getElementById('menu').style.display = 'none';
    document.getElementById('lobbyarea').style.display = 'block';

    imagesLoaded(g_jq, {}, function () {
        g_lobby = new WebLobby();
        g_lobby.begin();
        // game = new WebClient('player');
        // game.begin();
        // show_menu();
        // hide_join_buttons();
    });
};
