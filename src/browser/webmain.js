var game = {};

var g_sounds = {};

window.onload = function(){
    g_sounds[1] = new Howl({src: ['sound/cometome.wav',
                                'sound/cometome.mp3']});
    g_sounds[3] = new Howl({src: ['sound/followme.wav',
                                'sound/followme.mp3']});
    g_sounds[4] = new Howl({src: ['sound/intransit.wav',
                                'sound/intransit.mp3']});
    g_sounds[2] = new Howl({src: ['sound/splitup.wav',
                                'sound/splitup.mp3']});

    console.log(g_sounds);

    game = new WebClient();
    game.begin();
};
