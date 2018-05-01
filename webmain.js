var game = {};

var g_images = {};
var g_jq = [];

function load_image(name) {
    g_images[name] = new Image();
    g_images[name].src = "sprites/"+name+".png";
    g_jq.push(g_images[name]);
}

function start_game () {
    game = new WebClient();
    game.begin();
}

function reset_game () {
    if (game) game.sendReset();
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

    imagesLoaded(g_jq, {}, start_game);
};
