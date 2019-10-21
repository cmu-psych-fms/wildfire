function WebClient (mode) {
    this.mode = mode;
    this.engine = new GameEngine(new Config());
    this.network = {serverUpdates: [],
                    latency: 0};
    this.id = null;

    this.ticks = 0;
    this.time_debt = 0;

    this.keyState = {KEY_LEFT: 0,
                     KEY_RIGHT: 0,
                     KEY_UP: 0,
                     KEY_DOWN: 0,
                     KEY_SPACE: 0,
                     KEY_WP_WATER: 0,
                     KEY_WP_HOUS: 0,
                     KEY_WP_FIRE: 0,
                     KEY_WP_UNKNOWN: 0};

    this.keyEvents = [];

    this.movementRequests = [];
    this.curMovementRequestSeq = 0;

    this.retardantPredictions = [];
    this.predictedRetardantLevel = 0;

    this.map_images = new Array(10);
    this.map_images[MAP_FIRE] = g_images['fire'];
    this.map_images[MAP_ASH] = g_images['ash'];
    this.map_images[MAP_RETARDANT] = g_images['retardant'];
    this.map_images[MAP_WATER] = g_images['water'];
    this.map_images[MAP_TREE] = g_images['tree'];
    this.map_images[MAP_HOUSE] = g_images['house'];
    this.map_images[MAP_ROCK] = g_images['rock'];
    this.map_images[MAP_VROAD] = g_images['vroad'];
    this.map_images[MAP_HROAD] = g_images['hroad'];
    this.map_images[MAP_GRASS] = g_images['grass'];

    this.players = {};

    this.lastServerUpdate = null;
    this.currentServerUpdate = null;

    // Observer
    this.observer = {};
    this.observer.mode = 'ghost';
    this.observer.ghost = {};
    this.observer.ghost.x = 0;
    this.observer.ghost.y = 0;
    this.observer.ghost.dx = 0;
    this.observer.ghost.dy = 0;
    this.observer.follow = 0;

    // listeners
    this.listeners = {};
}

WebClient.prototype = {};

// WebClient.prototype.connect = function () {
//     this.network.socket = io.connect();
//     this.network.socket.on('connect', this.onConnect.bind(this));
//     this.network.socket.on('start', this.onStart.bind(this));

// };

WebClient.prototype.on = function (event, fn) {
    if (this.listeners[event]) {
        this.listeners[event].push = fn;
    } else {
        this.listeners[event] = [fn];
    }
};

WebClient.prototype.emit = function (event, data) {
    if (this.listeners[event]) {
        for (let i=0; i<this.listeners[event].length; i++) {
            this.listeners[event][i](data);
        }
    }
};

WebClient.prototype.begin = function (socket, gameState) {
    // So we can remove them later
    this.keyDownFunction = this.onKeyDown.bind(this);
    this.keyUpFunction = this.onKeyUp.bind(this);
    this.abortFunction = this.onClickAbort.bind(this);
    document.addEventListener('keydown', this.keyDownFunction);
    document.addEventListener('keyup', this.keyUpFunction);
    document.getElementById('abort').addEventListener('click', this.abortFunction);


    this.canvas = document.getElementById('gamecanvas');
    this.resizeCanvas();
    window.addEventListener('resize', this.resizeCanvas.bind(this));
    this.ctx = this.canvas.getContext('2d');

    this.setupGamepads();

    this.network.socket = socket;

    this.network.socket.on('disconnect', this.onDisconnect.bind(this));
    this.network.socket.on('update', this.onServerUpdate.bind(this));
    this.network.socket.on('join', this.onPlayerJoin.bind(this));
    this.network.socket.on('part', this.onPlayerPart.bind(this));
    this.network.socket.on('end', this.onEnd.bind(this));
    this.network.socket.on('viewport', this.onViewPort.bind(this));

    this.startTheGame(gameState);
};

WebClient.prototype.resizeCanvas = function () {
    console.log('resize');
    if (this.canvas) {
        // this.canvas.width = 710;
        // this.canvas.height = 630;
        // this.canvas.width = 600;
        // this.canvas.height = 500;
        this.canvas.width = 600;
        this.canvas.height = 500;
        this.canvas.width = Math.min(window.innerWidth, window.innerHeight);
        this.canvas.height = Math.min(window.innerWidth, window.innerHeight);
    }
};

WebClient.prototype.scanForGamepads = function () {
    var gp = navigator.getGamepads ? navigator.getGamepads() : (navigator.webkitGetGamepads ? navigator.webkitGetGamepads() : []);
    this.gamePads = [];
    for (let i=0; i<gp.length; i++)
        if (gp[i]) {
            if (gp[i].index in this.gamePads) this.gamePads[i] = gp[i];
            else this.gamePads.push(gp[i]);
        }
    this.gamePadState = new Array(this.gamePads.length);
    for (let i=0; i<this.gamePadState.length; i++)
        this.gamePadState[i] = {KEY_LEFT: false,
                                KEY_RIGHT: false,
                                KEY_UP: false,
                                KEY_DOWN: false,
                                KEY_SPACE: false,
                                KEY_WP_WATER: false}
    console.log('gamepads', this.gamePadState.length);
}

WebClient.prototype.connectGamepad = function (gamepad) {
    console.log('connect gp');
    this.scanForGamepads();
}

WebClient.prototype.disconnectGamepad = function (gamepad) {
    console.log('disconnect gp');
    this.scanForGamepads();
}

WebClient.prototype.setupGamepads = function () {
    var haveEvents = 'GamepadEvent' in window;
    var haveWebkitEvents = 'WebKitGamepadEvent' in window;
    this.scanForGamepads();
    if (haveEvents) {
        window.addEventListener("gamepadconnected", this.connectGamepad.bind(this));
        window.addEventListener("gamepaddisconnected", this.disconnectGamepad.bind(this));
    } else if (haveWebkitEvents) {
        window.addEventListener("webkitgamepadconnected", this.connectGamepad.bind(this));
        window.addEventListener("webkitgamepaddisconnected", this.disconnectGamepad.bind(this));
    } else {
        setInterval(scangamepads, 500);
    }
}

WebClient.prototype.pollGamepads = function () {
    var gp = navigator.getGamepads ? navigator.getGamepads() : (navigator.webkitGetGamepads ? navigator.webkitGetGamepads() : []);
    for (let i=0; i<gp.length; i++)
        if (gp[i] && gp[i].index in this.gamePads) this.gamePads[i] = gp[i];

    // for (let i=0; i<this.gamePads.length; i++) {
    //     for (j=0; j<this.gamePads[i].buttons.length; j++) {
    //         var val = this.gamePads[i].buttons[j];
    //         var pressed = val == 1.0;
    //         if (typeof(val) == "object") {
    //             pressed = val.pressed;
    //             val = val.value;
    //         }
    //         if (pressed) console.log('pressed', i, j);
    //     }
    // }

    for (let i=0; i<this.gamePads.length; i++) {
        if (this.gamePads[i].buttons[14].pressed && !this.gamePadState[i][KEY_LEFT]) this.pressGameKey(KEY_LEFT);
        if (this.gamePads[i].buttons[15].pressed && !this.gamePadState[i][KEY_RIGHT]) this.pressGameKey(KEY_RIGHT);
        if (this.gamePads[i].buttons[12].pressed && !this.gamePadState[i][KEY_UP]) this.pressGameKey(KEY_UP);
        if (this.gamePads[i].buttons[13].pressed && !this.gamePadState[i][KEY_DOWN]) this.pressGameKey(KEY_DOWN);
        if ((this.gamePads[i].buttons[0].pressed || this.gamePads[i].buttons[1].pressed || this.gamePads[i].buttons[2].pressed) && !this.gamePadState[i][KEY_SPACE]) this.pressGameKey(KEY_SPACE);
        if (this.gamePads[i].buttons[3].pressed && !this.gamePadState[i][KEY_WP_WATER]) this.pressGameKey(KEY_WP_WATER);

        if (!this.gamePads[i].buttons[14].pressed && this.gamePadState[i][KEY_LEFT]) this.releaseGameKey(KEY_LEFT);
        if (!this.gamePads[i].buttons[15].pressed && this.gamePadState[i][KEY_RIGHT]) this.releaseGameKey(KEY_RIGHT);
        if (!this.gamePads[i].buttons[12].pressed && this.gamePadState[i][KEY_UP]) this.releaseGameKey(KEY_UP);
        if (!this.gamePads[i].buttons[13].pressed && this.gamePadState[i][KEY_DOWN]) this.releaseGameKey(KEY_DOWN);
        if (!(this.gamePads[i].buttons[0].pressed || this.gamePads[i].buttons[1].pressed || this.gamePads[i].buttons[2].pressed) && this.gamePadState[i][KEY_SPACE]) this.releaseGameKey(KEY_SPACE);
        if (!this.gamePads[i].buttons[3].pressed && this.gamePadState[i][KEY_WP_WATER]) this.releaseGameKey(KEY_WP_WATER);

        this.gamePadState[i][KEY_LEFT] = this.gamePads[i].buttons[14].pressed;
        this.gamePadState[i][KEY_RIGHT] = this.gamePads[i].buttons[15].pressed;
        this.gamePadState[i][KEY_UP] = this.gamePads[i].buttons[12].pressed;
        this.gamePadState[i][KEY_DOWN] = this.gamePads[i].buttons[13].pressed;
        this.gamePadState[i][KEY_SPACE] = this.gamePads[i].buttons[0].pressed || this.gamePads[i].buttons[1].pressed || this.gamePads[i].buttons[2].pressed;
        this.gamePadState[i][KEY_WP_WATER] = this.gamePads[i].buttons[3].pressed;
    }
};

WebClient.prototype.decodeKeyCode = function(which) {
    if (which === 65 || which === 37)
        return KEY_LEFT;
    else if (which === 68 || which === 39)
        return KEY_RIGHT;
    else if (which === 87 || which === 38)
        return KEY_UP;
    else if (which === 83 || which === 40)
        return KEY_DOWN;
    else if (which === 32)
        return KEY_SPACE;
    else if (which === 49)
        return KEY_WP_WATER;
    // else if (which === 50)
    //     return KEY_WP_HOUSE;
    // else if (which === 51)
    //     return KEY_WP_FIRE;
    else
        return undefined;
};


WebClient.prototype.setObserverMode = function (mode, follow) {
    if (mode === 'ghost') {
        if (this.observer.mode === 'follow') {
            this.observer.ghost.x = this.players[this.observer.follow].position.x;
            this.observer.ghost.y = this.players[this.observer.follow].position.y;
        }
        this.observer.mode = 'ghost';
        this.observer.ghost.dx = 0;
        this.observer.ghost.dy = 0;
    } else if (mode === 'follow') {
        this.observer.mode = 'follow';
        this.observer.follow = follow;
    }
};

WebClient.prototype.cancelUpdates = function () {
    console.log("cancel animation");
    this.network.socket.off('disconnect');
    this.network.socket.off('update');
    this.network.socket.off('join');
    this.network.socket.off('part');
    this.network.socket.off('end');
    this.network.socket.off('viewport');

    document.removeEventListener('keydown', this.keyDownFunction);
    document.removeEventListener('keyup', this.keyUpFunction);
    document.getElementById('abort').removeEventListener('click', this.abortFunction);

    window.cancelAnimationFrame(this.updateid);
    this.stopGameTickTimer();
};

WebClient.prototype.pressGameKey = function (k) {
    if (!this.keyState[k]) {
        this.network.hasNewInput = true;
        this.keyEvents.push([1, k]);
        this.keyState[k] = 1;
    }
};

WebClient.prototype.releaseGameKey = function (k) {
    if (this.keyState[k]) {
        this.network.hasNewInput = true;
        this.keyEvents.push([0, k]);
        this.keyState[k] = 0;
    }
};


WebClient.prototype.onKeyDown = function (ev) {
    if (ev.which === 27) {
        this.cancelUpdates();
        this.network.socket.close();
    }

    if (this.mode === 'observer') {
        var used = false;
        if (ev.which === 65 || ev.which === 37) {
            if (this.observer.mode === 'ghost')
                this.observer.ghost.dx = -1;
            used = true;
        } else if (ev.which === 68 || ev.which === 39) {
            if (this.observer.mode === 'ghost')
                this.observer.ghost.dx = 1;
            used = true;
        } else if (ev.which === 87 || ev.which === 38) {
            if (this.observer.mode === 'ghost')
                this.observer.ghost.dy = -1;
            used = true;
        } else if (ev.which === 83 || ev.which === 40) {
            if (this.observer.mode === 'ghost')
                this.observer.ghost.dy = 1;
            used = true;
        } else if (ev.which >= 49 && ev.which <= 59) {
            // follow the player
            var i = ev.which - 49;
            var f = Object.keys(this.players)[i];
            if (f) this.setObserverMode('follow', f);
            used = true;
        } else if (ev.which === 48) {
            this.setObserverMode('ghost');
            used = true;
        }
        if (used) {
            ev.preventDefault();
            ev.stopPropagation();
        }
    } else {
        var k = this.decodeKeyCode(ev.which);
        if (k) {
            if (!this.keyState[k]) {
                this.network.hasNewInput = true;
                this.keyEvents.push([1, k]);
            }
            ev.preventDefault();
            ev.stopPropagation();
            this.keyState[k] = 1;
        }
    }
};

WebClient.prototype.onKeyUp = function (ev) {
    if (this.mode === 'observer') {
        var used = false;
        if (ev.which === 65 || ev.which === 37) {
            if (this.observer.mode === 'ghost' && this.observer.ghost.dx === -1)
                this.observer.ghost.dx = 0;
            used = true;
        } else if (ev.which === 68 || ev.which === 39) {
            if (this.observer.mode === 'ghost' && this.observer.ghost.dx === 1)
                this.observer.ghost.dx = 0;
            used = true;
        } else if (ev.which === 87 || ev.which === 38) {
            if (this.observer.mode === 'ghost' && this.observer.ghost.dy === -1)
                this.observer.ghost.dy = 0;
            used = true;
        } else if (ev.which === 83 || ev.which === 40) {
            if (this.observer.mode === 'ghost' && this.observer.ghost.dy === 1)
                this.observer.ghost.dy = 0;
            used = true;
        } else if (ev.which >= 49 && ev.which < 59) {
            // follow the player
            var i = ev.which - 49;
            var f = Object.keys(this.players)[i];
            if (f) {
                this.observer.mode = 'follow';
                this.observer.follow = f;
            }
            used = true;
        } else if (ev.which === 59) {
            this.observer.mode = 'ghost';
            this.observer.ghost.dx = 0;
            this.observer.ghost.dy = 0;
            used = true;
        }
        if (used) {
            ev.preventDefault();
            ev.stopPropagation();
        }
    } else {
        var k = this.decodeKeyCode(ev.which);
        if (k) {
            if (this.keyState[k]) {
                this.network.hasNewInput = true;
                this.keyEvents.push([0, k]);
            }
            ev.preventDefault();
            ev.stopPropagation();
            this.keyState[k] = 0;
        }
    }
};

WebClient.prototype.onClickAbort = function () {
    this.network.socket.emit('abort');
};

WebClient.prototype.startGameTickTimer = function () {
    this.gameTickTimer = setInterval(this.gameLogicUpdate.bind(this), 15);
};

WebClient.prototype.stopGameTickTimer = function () {
    clearInterval(this.gameTickTimer);
};

WebClient.prototype.initPlayer = function (k) {
    this.players[k] = { last: {position: {x:0,y:0},
                               angle: 0,
                               turnFlag: null,
                               speed: 0,
                               alive: true,
                               water: 0,
                               ts: 0,
                               s_tick: 0,
                               c_tick: 0,
                              },
                        latest: {position: {x:0,y:0},
                                 angle: 0,
                                 turnFlag: null,
                                 speed: 0,
                                 alive: true,
                                 water: 0,
                                 ts: 0,
                                 s_tick: 0,
                                 c_tick: 0},
                        lerp: 0,
                        position: {x:0,y:0},
                        speed: 0,
                        angle: 0,
                        water: 0,
                        alive: true,
                        turnFlag: 0,
                        thrustFlag: 0,
                        dumpFlag: 0,
                        wayPointFlags: [],
                        config: this.engine.config.player
                      };
};

WebClient.prototype.updatePlayerLatest = function (p, serverTick, clientTick, update) {
    p.last.position.x = p.latest.position.x;
    p.last.position.y = p.latest.position.y;
    p.last.angle = p.latest.angle;
    p.last.turnFlag = p.latest.turnFlag;
    p.last.speed = p.latest.speed;
    p.last.alive = p.latest.alive;
    p.last.water = p.latest.water;
    p.last.ts = p.latest.ts;
    p.last.s_tick = p.latest.s_tick;
    p.last.c_tick = p.latest.c_tick;

    p.latest.ts = null;
    p.latest.position.x = update[1];
    p.latest.position.y = update[2];
    p.latest.angle = update[3];
    p.latest.speed = update[4];
    p.latest.turnFlag = update[5];
    p.latest.water = update[6];
    p.latest.s_tick = serverTick;
    p.latest.c_tick = clientTick;
};

WebClient.prototype.setPlayerCurrent = function (p) {
    p.position.x = p.latest.position.x;
    p.position.y = p.latest.position.y;
    p.angle = p.latest.angle;
    // p.turnFlag = p.latest.turnFlag;
    p.speed = p.latest.speed;
    p.water = p.latest.water;
};

WebClient.prototype.onConnect = function () {
    this.network.socket.emit('greet', {'mode': this.mode});
};

WebClient.prototype.startTheGame = function (data) {
    console.log('start', data);
    for (let k in data.players) {
        this.initPlayer(k);
        var p = this.players[k];

        console.log('before', p);
        p.alive = data.players[k][0];
        p.position.x = data.players[k][1];
        p.position.y = data.players[k][2];
        p.angle = data.players[k][3];
        p.speed = data.players[k][4];
        p.turnFlag = data.players[k][5];
        p.water = data.players[k][6];

        p.last.position.x = p.position.x;
        p.last.position.y = p.position.y;
        p.last.angle = p.angle;
        p.last.ts = 0;
        p.last.tick = 0;

        p.latest.position.x = p.position.x;
        p.latest.position.y = p.position.y;
        p.latest.angle = p.angle;
        p.latest.ts = 0;
        p.latest.tick = 0;

        // console.log(data.players[k][1], data.players[k][2], data.players[k][3]);

        // console.log(this.engine.players[k]);
    }
    // this.engine.smoke = data.smoke;
    // Players have IDs and observers don't.
    this.mode = data.id ? 'player':'observer';

    this.engine.map = data.map;
    console.log('map', this.engine.map.width, this.engine.map.height, this.engine.map.fire.length);

    if (this.mode === 'observer') {
        this.observer.mode = 'ghost';
        this.observer.ghost.x = this.engine.map.width * this.engine.config.map.cellSize / 2;
        this.observer.ghost.y = this.engine.map.width * this.engine.config.map.cellSize / 2;
    } else {
        this.id = data.id;
    }

    this.startGameTickTimer();

    this.updateDisplay( new Date().getTime() );
};

WebClient.prototype.onDisconnect = function (data) {
    console.log('disconnect', data);
    // We got disconnected
    this.network.state = 'disconnected';
    this.cancelUpdates();
};


WebClient.prototype.sendReset = function () {
    this.network.socket.emit('reset');
}

WebClient.prototype.sendPing = function () {
    var ts = new Date().getTime();
    this.network.socket.emit('ping', ts);
};

WebClient.prototype.onPing = function (ts) {
    console.log('ping', ts);
    this.network.latency = new Date().getTime() - ts;
    console.log('latency is ', this.network.latency);
};

WebClient.prototype.onPlayerJoin = function (data) {
    console.log('join', data);
    this.initPlayer(data.id);
};

WebClient.prototype.onPlayerPart = function (data) {
    console.log('part', data);
    if (this.mode === 'observer' &&
        this.observer.mode === 'follow' &&
        this.observer.follow === data.id) {
        this.setObserverMode('ghost');
    }

    delete this.players[data.id];
};

WebClient.prototype.onEnd = function (data) {
    console.log('game over', data);
    this.cancelUpdates();
    this.emit('end');
};

WebClient.prototype.onServerUpdate = function (data) {
    this.network.serverUpdates.push(data);
    if (this.network.serverUpdates.length >= this.engine.config.serverUpdateBufferSize) {
        this.network.serverUpdates.splice(0,1);
    }
};

WebClient.prototype.onViewPort = function (data) {
    this.engine.map.viewPort = data;
};

WebClient.prototype.processKbdInput = function () {
    var p = this.players[this.id];

    if (this.keyEvents.length > 0) {
        this.engine.processPlayerKeysHelper(this.players[this.id], this.keyEvents);
        this.keyEvents = [];
    }
};

WebClient.prototype.lerpOtherPlayers = function () {
    for (let k in this.players) {
        if (k === this.id) continue;
        var p = this.players[k];

        var ofs = this.ticks - p.latest.c_tick;
        var dist = p.latest.s_tick-p.last.s_tick;

        // console.log('lerp', ofs, dist);

        if (ofs >= dist) {
            this.setPlayerCurrent(p);
        } else if (dist > 0) {
            p.position.x = p.last.position.x + (p.latest.position.x - p.last.position.x) * ofs / dist;
            p.position.y = p.last.position.y + (p.latest.position.y - p.last.position.y) * ofs / dist;
            p.angle = stdAngle(p.last.angle + angle_diff(p.latest.angle,p.last.angle) * ofs / dist);
            // Not sure how to lerp water usage.
            p.water = p.latest.water;
        }
    }
};

WebClient.prototype.somethingChanged = function (p) {
    // var last = this.movementRequests[this.movementRequests.length-1];
    // return (p.turnFlag || p.thrustFlag || p.dumpFlag ||
    //         p.speed > 0 ||
    //         (this.movementRequests.length > 0 &&
    //          (p.turnFlag !== last[1] ||
    //           p.thrustFlag !== last[2] ||
    //           p.dumpFlag !== last[3])));
    return true;
};

WebClient.prototype.placeMovementRequest = function () {
    var p = this.players[this.id];
    if (this.somethingChanged(p)) {
        this.curMovementRequestSeq += 1;
        // this.keyEventsTick = this.ticks;
        // var k = {seq:this.keyEventsSeq, tick: this.ticks, events:this.keyEvents};
        if (p.wayPointFlags.length > 0) console.log(p.wayPointsFlags);
        var k = [ this.curMovementRequestSeq, p.turnFlag, p.thrustFlag, p.dumpFlag, p.wayPointFlags ];
        // console.log(k);
        this.movementRequests.push(k);
        this.network.socket.emit('movementRequest', k);
        p.wayPointFlags.length = 0;
    }
};

WebClient.prototype.interpolatePlayer = function (p) {
    // console.log('lerp', p.lerp.step, p.lerp.lastKnown.a, p.lerp.target.a, p.angle);
    p.lerp.step += 1;
    if (p.lerp.step >= p.lerp.steps) {
        p.position.x = p.lerp.target.x;
        p.position.y = p.lerp.target.y;
        p.angle = p.lerp.target.a;
        p.lerp = null;
    } else {
        p.position.x = p.lerp.lastKnown.x + (p.lerp.target.x-p.lerp.lastKnown.x)/p.lerp.steps*p.lerp.step;
        p.position.y = p.lerp.lastKnown.y + (p.lerp.target.y-p.lerp.lastKnown.y)/p.lerp.steps*p.lerp.step;
        p.angle = stdAngle(p.lerp.lastKnown.a + angle_diff(p.lerp.target.a,p.lerp.lastKnown.a)/p.lerp.steps*p.lerp.step);
    }
};

// WebClient.prototype.mapAtUnsafe = function (x, y) {
//     var r = this.engine.map.data[y*this.engine.map.width+x];
//     for (let i=0; i<this.mapChangePredictions.length; i++) {
//         if (this.mapChangePredictions[i][0] === x &&
//             this.mapChangePredictions[i][1] === y)
//             r = this.mapChangePredictions[i][2];
//     }
//     return r;
// };

WebClient.prototype.addRetardant = function (x, y) {
    if (x>=0 && x < this.engine.map.width &&
        y>=0 && y < this.engine.map.height &&
        this.predictedRetardantLevel > 0) {
        var m = this.engine.mapAt(x,y);
        if (m !== MAP_WATER) {
            for (let i=0; i<this.retardantPredictions; i++) {
                if (this.retardantPredictions[i][0] === x &&
                    this.retardantPredictions[i][0] === y) {
                    return;
                }
            }
            // console.log("retardant", this.predictedRetardantLevel, this.retardantPredictions.length);
            this.retardantPredictions.push([x, y]);
            this.predictedRetardantLevel -= 1;
        }
    }
};

WebClient.prototype.stepPlayer = function (p) {
    if (p.alive) {
        this.engine.playerApplyMovement(p);

        if (p.dumpFlag) {
            var mx = Math.round(p.position.x/this.engine.config.map.cellSize),
                my = Math.round(p.position.y/this.engine.config.map.cellSize);
                // m = this.engine.mapAt(mx, my);
            this.addRetardant(mx, my);
        }
    }
}

WebClient.prototype.replayPlayerKeys = function () {
    var p = this.players[this.id];
    for (let i=0; i<this.movementRequests.length; i++) {
        p.turnFlag = this.movementRequests[i][1];
        p.thrustFlag = this.movementRequests[i][2];
        p.dumpFlag = this.movementRequests[i][3];
        this.stepPlayer(p);
    }
    if (p.angle !== p.backup_a) {
        console.log('backup differs', p.angle, p.backup_a);
    }
};

WebClient.prototype.processServerUpdates = function () {
    if (this.network.serverUpdates.length > 0) {
        // Need to run through all updates for map changes
        for (let i=0; i<this.network.serverUpdates.length; i++) {
            var mapUpdates = this.network.serverUpdates[i].m;
            for (let i=0; i<mapUpdates.length;i++) {
                this.engine.map.data[mapUpdates[i][0]] = mapUpdates[i][1];
            }

            // this.engine.fireSanity('before');

            var fireUpdates = this.network.serverUpdates[i].f;
            for (let i=0; i<fireUpdates.length;i++) {
                var f = fireUpdates[i][1];
                if (fireUpdates[i][0] === UPDATE_ADD) {
                    this.engine.map.fire.push(f);
                } else {
                    for (let j=this.engine.map.fire.length-1; j>=0; j--) {
                        if (f.x === this.engine.map.fire[j].x &&
                            f.y === this.engine.map.fire[j].y) {
                            if (fireUpdates[i][0] === UPDATE_DEL) {
                                this.engine.map.fire.splice(j,1);
                            } else if (fireUpdates[i][0] === UPDATE_UPDATE) {
                                this.engine.map.fire[j] = f;
                            } else {
                                console.log("no action for fire update", fireUpdates[i]);
                            }
                            break;
                        }
                    }
                }
            }

            // this.engine.fireSanity('after');

            var retardantUpdates = this.network.serverUpdates[i].r;
            for (let i=0; i<retardantUpdates.length;i++) {
                var f = retardantUpdates[i][1];
                if (retardantUpdates[i][0] === UPDATE_ADD) {
                    this.engine.map.retardant.push(f);
                } else {
                    for (let j=0; j<this.engine.map.retardant.length; j++) {
                        if (f.x === this.engine.map.retardant[j].x &&
                            f.y === this.engine.map.retardant[j].y) {
                            if (retardantUpdates[i][0] === UPDATE_DEL) {
                                this.engine.map.retardant.splice(j,1);
                            } else if (retardantUpdates[i][0] === UPDATE_UPDATE) {
                                this.engine.map.retardant[j] = retardantUpdates[i][1];
                            } else {
                                console.log("no action for retardant update", retardantUpdates[i]);
                            }
                            break;
                        }
                    }
                }
            }


            var wayPointUpdates = this.network.serverUpdates[i].wp;
            for (let i=0; i<wayPointUpdates.length;i++) {
                var wp = wayPointUpdates[i][1];
                if (wayPointUpdates[i][0] === UPDATE_ADD) {
                    this.engine.map.wayPoints.push(wp);
                } else {
                    for (let j=0; j<this.engine.map.retardant.length; j++) {
                        if (wp.x === this.engine.map.retardant[j].x &&
                            wp.y === this.engine.map.retardant[j].y) {
                            if (wayPointUpdates[i][0] === UPDATE_DEL) {
                                this.engine.map.wayPoints.splice(j,1);
                            } else if (wayPointUpdates[i][0] === UPDATE_UPDATE) {
                                this.engine.map.wayPoints[j] = wayPointUpdates[i][1];
                            } else {
                                console.log("no action for waypoint update", wayPointUpdates[i]);
                            }
                            break;
                        }
                    }
                }
            }

            if (typeof this.network.serverUpdates[i].vp !== 'undefined') {
                this.engine.map.viewPort = this.network.serverUpdates[i].vp;
                // console.log('view port updated', this.engine.map.viewPort);
            }
        }
        var last = this.network.serverUpdates[this.network.serverUpdates.length-1];
        this.engine.ticks = last.t;

        this.lastServerUpdate = this.currentServerUpdate;
        this.currentServerUpdate = last;
        this.currentServerUpdate.localTimeStamp = this.lastUpdateTime;

        // But we only need the last update for player positions
        var p_u = last.p;
        for (let k in p_u) {
            var p = this.players[k];
            if (p) {
                this.updatePlayerLatest(p, last.t, this.ticks, p_u[k]);
            }
        }
        // Erase uneeded key events
        for (let i=0; i<this.movementRequests.length; i++) {
            if (this.movementRequests[i][0] === this.currentServerUpdate.lms) {
                this.movementRequests.splice(0,i+1);
                break;
            }
        }
        this.retardantPredictions.length = 0;

        if (this.mode === 'player') {

            this.predictedRetardantLevel = this.players[this.id].latest.water;

            this.players[this.id].backup_x = this.players[this.id].position.x;
            this.players[this.id].backup_y = this.players[this.id].position.y;
            this.players[this.id].backup_a = this.players[this.id].angle;
            this.players[this.id].backup_speed = this.players[this.id].speed;

            this.setPlayerCurrent(this.players[this.id]);
            // replay our key events from the last authoritative game state.
            this.replayPlayerKeys();
        }

        this.network.serverUpdates.length = 0;

    // console.log('fire', this.engine.map.fire.length, 'retardant', this.engine.map.retardant.length);

    }
};

WebClient.prototype.drawExplosion = function(ctx, x, y) {
    var ofs = 0;
    var radius;
    for (radius=15; radius<70; radius+=8) {
        var angle;
        ofs += 3;
        if (radius < 60) { ctx.strokeStyle = '#FFFF00'; }
        else { ctx.strokeStyle = '#FF0000'; }
        for (angle=0; angle<360; angle += 30) {
            ctx.beginPath();
            ctx.arc(x, y, radius, deg2rad(angle+ofs),deg2rad(angle+ofs+10),false);
            ctx.stroke();
        }
    }
    ctx.strokeStyle = '#FFFF00';
    ctx.beginPath();
    ctx.arc(x, y, 7, 0, Math.PI*2, false);
    ctx.stroke();
};

WebClient.prototype.HUDDirection = function (from, to) {
    var a = stdAngle(angleTo(from, to));
    var p1 = {x:this.canvas.width/2, y:this.canvas.height/2};
    var p2 = {x:p1.x + Math.cos(deg2rad(a)),
              y:p1.y + Math.sin(deg2rad(a))};
    var p3, p4;

    var corners = [rad2deg(Math.atan2(this.canvas.height/2,this.canvas.width/2)),
                   rad2deg(Math.atan2(this.canvas.height/2,-this.canvas.width/2)),
                   stdAngle(rad2deg(Math.atan2(-this.canvas.height/2,-this.canvas.width/2))),
                   stdAngle(rad2deg(Math.atan2(-this.canvas.height/2,this.canvas.width/2)))];

    // console.log(corners);

    // start_a = 45;

    var x,y;
    // console.log(a);
    if (a <= corners[0]) {
        p3 = {x:this.canvas.width-5, y: 5};
        p4 = {x:this.canvas.width-5, y: this.canvas.height-5};
    } else if (a <= corners[1]) {
        p3 = {x:5, y: this.canvas.height-5};
        p4 = {x:this.canvas.width-5, y: this.canvas.height-5};
    } else if (a <= corners[2]) {
        p3 = {x:5, y: 5};
        p4 = {x:5, y: this.canvas.height-5};
    } else if (a <= corners[3]) {
        p3 = {x:5, y:5};
        p4 = {x:this.canvas.width-5, y:5};
    } else {
        p3 = {x:this.canvas.width-5, y:5};
        p4 = {x:this.canvas.width-5, y:this.canvas.height-5};
    }
    return lines_intersection_point(p1,p2,p3,p4)[1];
};

WebClient.prototype.drawWayPointsOnMap = function () {
    for (let i=0; i<this.engine.map.wayPoints.length; i++) {
        if (Math.abs(this.players[this.id].position.x - this.engine.map.wayPoints[i].x) < this.canvas.width/2 &&
            Math.abs(this.players[this.id].position.y - this.engine.map.wayPoints[i].y) < this.canvas.height/2) {
            var letter;
            switch(this.engine.map.wayPoints[i].type) {
            case MAP_WATER:
                this.ctx.fillStyle = "#3333FF";
                letter = "WATER";
                break;
            case MAP_HOUSE:
                this.ctx.fillStyle = "#BB9900";
                letter = "HOUSE";
                break;
            case MAP_FIRE:
                this.ctx.fillStyle = "#FFFF00";
                letter = "FIRE";
                break;
            default:
                this.ctx.fillStyle = "#FFFFFF";
                letter = "????";
                break;
            }
            var x = this.engine.map.wayPoints[i].x,
                y = this.engine.map.wayPoints[i].y;
            var w = 40, h=20;
            this.ctx.beginPath();
            this.ctx.moveTo(x-w,y-h*3);
            this.ctx.lineTo(x+w,y-h*3);
            this.ctx.lineTo(x+w,y-h);
            this.ctx.lineTo(x+w*1/3,y-h);
            this.ctx.lineTo(x,y);
            this.ctx.lineTo(x-w/3,y-h);
            this.ctx.lineTo(x-w,y-h);
            this.ctx.globalAlpha = 0.6;
            this.ctx.fill()
            this.ctx.fillStyle = "#FFFFFF";
            this.ctx.textAlign = 'center';
            this.ctx.textBaseline = 'middle';
            this.ctx.fillText(letter,
                              this.engine.map.wayPoints[i].x,
                              this.engine.map.wayPoints[i].y-h*2);
            this.ctx.globalAlpha = 1;
        }
    }
};

WebClient.prototype.drawWayPointsOnHUD = function (centerPos) {
    for (let i=0; i<this.engine.map.wayPoints.length; i++) {
        if (Math.abs(centerPos.x - this.engine.map.wayPoints[i].x) < this.canvas.width/2 &&
            Math.abs(centerPos.y - this.engine.map.wayPoints[i].y) < this.canvas.height/2)
            continue;
        switch(this.engine.map.wayPoints[i].type) {
        case MAP_WATER:
            this.ctx.fillStyle = "#3333FF";
            letter = "W";
            break;
        case MAP_HOUSE:
            this.ctx.fillStyle = "#BB9900";
            letter = "H";
            break;
        case MAP_FIRE:
            this.ctx.fillStyle = "#FFFF00";
            letter = "F";
            break;
        default:
            this.ctx.fillStyle = "#FFFFFF";
            letter = "?";
            break;
        }

        var pos = this.HUDDirection(centerPos, this.engine.map.wayPoints[i]);
        this.ctx.beginPath();
        this.ctx.arc(pos.x, pos.y, 5, 0, Math.PI*2);
        this.ctx.fill();
    }
};

WebClient.prototype.drawPlayersOnHUD = function (centerPos, thisPlayerId) {
    for (var id in this.players) {
        if (id === thisPlayerId) continue;
        if (Math.abs(this.players[id].position.x - centerPos.x) < this.canvas.width/2 &&
            Math.abs(this.players[id].position.y - centerPos.y) < this.canvas.height/2)
            continue;

        var pos = this.HUDDirection(centerPos, this.players[id].position);
        this.ctx.beginPath();
        this.ctx.fillStyle = "#BC9058";
        this.ctx.fillRect(pos.x-4, pos.y-4,8,8);
        // this.ctx.fillRect(pos.x-5, pos.y-5,10,10);
    }
}

WebClient.prototype.reportNaN = function(where) {
    for (let id in this.players) {
        if (isNaN(this.players[id].position.x) || isNaN(this.players[id].position.y))
            console.log(where, 'NaN', id, this.players[id].position.x, this.players[id].position.y);
    }
};

WebClient.prototype.drawGameState = function () {
    var thisPlayerId, centerPos;


    if (this.mode === 'observer') {
        if (this.observer.mode === 'ghost') {
            centerPos = this.observer.ghost;
        } else if (this.observer.mode === 'follow') {
            centerPos = this.players[this.observer.follow].position;
            thisPlayerId = this.observer.follow;
        } else {
            centerPos = {x:0,y:0};
        }
    } else {
        centerPos = this.players[this.id].position;
        thisPlayerId = this.id;
    }

    this.ctx.save();
    this.ctx.clearRect(0,0,this.canvas.width,this.canvas.height);
    this.ctx.translate(Math.round(-centerPos.x+this.canvas.width/2),
                       Math.round(-centerPos.y+this.canvas.height/2));
    // this.ctx.strokeStyle = '#003300';
    var maxx = this.engine.map.width * this.engine.config.map.cellSize;
    var maxy = this.engine.map.height * this.engine.config.map.cellSize;
    // for (let x=0; x<this.engine.mapWidth; x++) {
    //     this.ctx.moveTo(x*this.engine.config.map.cellSize, 0);
    //     this.ctx.lineTo(x*this.engine.config.map.cellSize, maxy);
    // }
    // for (let y=0; y<this.engine.mapHeight; y++) {
    //     this.ctx.moveTo(0, y*this.engine.config.map.cellSize);
    //     this.ctx.lineTo(maxx, y*this.engine.config.map.cellSize);
    // }
    // this.ctx.stroke();

    var startx = Math.floor((centerPos.x - this.canvas.width/2)/this.engine.config.map.cellSize);
    var starty = Math.floor((centerPos.y - this.canvas.height/2)/this.engine.config.map.cellSize);
    var endx = startx+Math.ceil(this.canvas.width/this.engine.config.map.cellSize);
    var endy = starty+Math.ceil(this.canvas.height/this.engine.config.map.cellSize);

    if (startx < 0) startx = 0;
    if (starty < 0) starty = 0;
    if (endx >= this.engine.map.width) endx = this.engine.map.width-1;
    if (endy >= this.engine.map.height) endy = this.engine.map.height-1;

    if (startx < this.engine.map.viewPort.x) startx = this.engine.map.viewPort.x;
    if (starty < this.engine.map.viewPort.y) starty = this.engine.map.viewPort.y;
    if (endx >= this.engine.map.viewPort.x + this.engine.map.viewPort.w) endx = this.engine.map.viewPort.x + this.engine.map.viewPort.w;
    if (endy >= this.engine.map.viewPort.y + this.engine.map.viewPort.h) endy = this.engine.map.viewPort.y + this.engine.map.viewPort.h;


    // console.log(startx, endx, starty, endy);

    for (let y=starty; y<=endy; y++) {
        for (let x=startx; x<=endx; x++) {
            var m = this.map_images[this.engine.map.data[y*this.engine.map.width+x]];
            if (!m) console.log('nonexistent', this.engine.map.data[y*this.engine.map.width+x], x, y);
            try {
            this.ctx.drawImage(m,
                               x * this.engine.config.map.cellSize,
                               y * this.engine.config.map.cellSize);
            } catch(e) {
                console.log('oops', m);
                console.log(this.map_images.length, this.engine.map.data[y*this.engine.map.width+x], this.engine.map.data.length, y*this.engine.map.width+x);
                throw(e);
            }

        }
    }

    // Map Border
    this.ctx.lineWidth = 4;
    this.ctx.strokeStyle = '#00AA00';
    this.ctx.strokeRect(this.engine.map.viewPort.x*this.engine.config.map.cellSize-2, this.engine.map.viewPort.y*this.engine.config.map.cellSize-2, this.engine.map.viewPort.w*this.engine.config.map.cellSize+4, this.engine.map.viewPort.h*this.engine.config.map.cellSize+4);
    this.ctx.lineWidth = 1;


    // Fire
    for (let i=0; i<this.engine.map.fire.length; i++) {
        this.ctx.drawImage(Math.random() < 0.5 ? g_images['flame']:g_images['flame2'],
                           this.engine.map.fire[i].x * this.engine.config.map.cellSize,
                           this.engine.map.fire[i].y * this.engine.config.map.cellSize);
    }

    for (let i=0; i<this.engine.map.retardant.length; i++) {
        this.ctx.drawImage(this.map_images[MAP_RETARDANT],
                           this.engine.map.retardant[i].x * this.engine.config.map.cellSize,
                           this.engine.map.retardant[i].y * this.engine.config.map.cellSize);
    }

    this.ctx.strokeStyle = "#FF0000";
    this.ctx.lineWidth = 2;
    for (let i=0; i<this.retardantPredictions.length; i++) {
        if (this.retardantPredictions[i][0] >= startx &&
            this.retardantPredictions[i][0] <= endx &&
            this.retardantPredictions[i][1] >= starty &&
            this.retardantPredictions[i][1] <= endy) {
            this.ctx.drawImage(this.map_images[MAP_RETARDANT],
                               this.retardantPredictions[i][0] * this.engine.config.map.cellSize,
                               this.retardantPredictions[i][1] * this.engine.config.map.cellSize);
            this.ctx.strokeRect(this.retardantPredictions[i][0] * this.engine.config.map.cellSize,
                                this.retardantPredictions[i][1] * this.engine.config.map.cellSize,
                                this.engine.config.map.cellSize,
                                this.engine.config.map.cellSize);
        }
    }
    this.ctx.lineWidth = 1;

    this.ctx.font = "10pt sans-serif";
    this.ctx.textAlign = "center";
    this.ctx.textBaseLine = "middle";

    for (let id in this.players) {
        if (this.players[id].alive) {
            this.ctx.save();
            this.ctx.translate(this.players[id].position.x,
                               this.players[id].position.y);
            this.ctx.rotate(deg2rad(this.players[id].angle));
            this.ctx.drawImage(g_images['plane'], -g_images['plane'].width/2-10, -g_images['plane'].height/2);
            var engine_flames = 0;
            if (this.players[id].health < 50) {
                engine_flames = 4;
            } else if (this.players[id].health < 100) {
                engine_flames = 3;
            } else if (this.players[id].health < 150) {
                engine_flames = 2;
            } else if (this.players[id].health < 250) {
                engine_flames = 1;
            }
            this.ctx.beginPath();
            var r = Math.round(150+Math.random() * 55);
            this.ctx.strokeStyle = 'rgb('+r+','+r+',0)';
            // engine_flames = 4;
            var y_lookup = [30, 15, -15, -30];
            for (let f=0;f<engine_flames;f++){
                for (let l=0;l<5; l++) {
                    this.ctx.moveTo(-40 + Math.random()*8-4,
                                    y_lookup[f] + Math.random()*10-5);
                    this.ctx.lineTo(-19 + Math.random()*8-4,
                                    y_lookup[f]+ Math.random()*4-2);
                }
            }
            this.ctx.stroke();

            this.ctx.restore();

            this.ctx.beginPath();
            this.ctx.arc(this.players[id].position.x, this.players[id].position.y, 6,
                         0, Math.PI*2);
            this.ctx.fillStyle = '#AA2200';
            this.ctx.fill();
            var n = Object.keys(this.players).indexOf(id) + 1;
            this.ctx.fillStyle = '#FFFFFF';
            this.ctx.fillText(n.toString(), this.players[id].position.x, this.players[id].position.y+4, 5);


        }
            // shipWireframe.draw (this.ctx,
            //                     this.players[id].position.x,
            //                     this.players[id].position.y,
            //                     this.players[id].angle);
        else
            this.drawExplosion(this.ctx,
                               this.players[id].position.x,
                               this.players[id].position.y);
    }

    // var alpha = 0.90;
    // var fadein = 20.0;
    // var fadeout = 50.0;
    // for (let i=0; i<this.engine.smoke.length; i++) {
    //     var s = this.engine.smoke[i];
    //     var dur = this.engine.config.smoke.duration + this.engine.smoke[i].tick - this.engine.ticks;
    //     if (dur < 0) continue;
    //     else if (dur < fadeout)
    //         this.ctx.globalAlpha = dur / fadeout * alpha;
    //     else if (dur > this.engine.config.smoke.duration - fadein) {
    //         this.ctx.globalAlpha =  alpha * (this.engine.config.smoke.duration-dur) / fadein;
    //         // this.ctx.globalAlpha = alpha - (this.engine.config.smoke.duration - dur) / fadein * alpha;
    //     } else
    //             this.ctx.globalAlpha = alpha;
    //     this.ctx.save();
    //     this.ctx.translate(s.position.x, s.position.y);
    //     this.ctx.rotate(dur/300);
    //     this.ctx.translate(-g_images.smoke.width/2, -g_images.smoke.height/2);
    //     this.ctx.drawImage(g_images.smoke,
    //                        // -g_images.smoke.width/2 + s.position.x + s.velocity.x * (s.ticks-this.engine.ticks),
    //                        // g_images.smoke.height/2 + s.position.y + s.velocity.y * (s.ticks-this.engine.ticks));
    //                        0,
    //                        0);
    //     this.ctx.restore();

    // }
    // this.ctx.globalAlpha = 1;

    this.drawWayPointsOnMap();

    this.ctx.restore();

    // HUD
    this.ctx.fillStyle = '#000000';
    this.ctx.fillRect(0,0,this.canvas.width,10);
    this.ctx.fillRect(0,this.canvas.height-10,this.canvas.width,10);
    this.ctx.fillRect(0,10,10,this.canvas.height-20);
    this.ctx.fillRect(this.canvas.width-10,10,10,this.canvas.height-20);

    this.drawWayPointsOnHUD(centerPos);
    this.drawPlayersOnHUD(centerPos, thisPlayerId);

    if (thisPlayerId) {
        var water_w = 200;
        var water_y = 20;
        var water_x = (this.canvas.clientWidth - water_w)/2;
        this.ctx.strokeStyle = "#3333FF";
        this.ctx.strokeRect(water_x, water_y, water_w, 20);
        this.ctx.fillStyle = "#AAAAFF";
        this.ctx.fillRect(water_x, water_y+1, water_w, 18);
        this.ctx.fillStyle = "#3333FF";
        this.ctx.fillRect(water_x, water_y+1, water_w * this.players[thisPlayerId].water / this.engine.config.player.maxWater , 18);

        this.ctx.fillStyle = "#FF0000"
        this.ctx.font = "14pt sans-serif";
        this.ctx.textAlign = "center";
        this.ctx.fillText(this.engine.map.fire.length.toString(), this.canvas.clientWidth/2, this.canvas.clientHeight-17);
    }

    // for (let i=1; i<= this.engine.config.player.maxWater; i++) {
    //     var full = i<=this.players[this.id].water;
    //     if (full)
    //         this.ctx.fillRect();
    //     else
    //         this.ctx.fillRect();
    //     this.ctx.drawImage( ? g_images['watertankfull']:g_images['watertankempty'],
    //                        2 + (i-1)*(g_images['watertankfull'].width+3),
    //                        5);
    // }


};

WebClient.prototype.moveObserver = function () {
    if (this.observer.mode === 'ghost') {
        this.observer.ghost.x += this.observer.ghost.dx * 5;
        this.observer.ghost.y += this.observer.ghost.dy * 5;
    }
};

WebClient.prototype.gameLogicUpdate = function () {
    var t = new Date().getTime();
    this.dt = this.lastUpdateTime ? (t - this.lastUpdateTime) : 1000/60.0;
    this.lastUpdateTime = t;

    if (this.mode === 'observer') {
        this.ticks += 1;

        this.processServerUpdates();
        this.processKbdInput();
        this.pollGamepads();
        this.lerpOtherPlayers();
        this.moveObserver();
    } else {
        var ms = this.dt + this.time_debt;
        var dur = 15;
        // run multiple game steps if needed to maintain a 15ms game
        // tick rate.
        var count = 0;
        // Limit the number of steps to 60. any more and there may be
        // responsiveness issues.
        while (ms > 0 && count < 60) {
            this.ticks += 1;

            this.processServerUpdates();
            this.processKbdInput();
            this.pollGamepads();
            this.placeMovementRequest();
            this.stepPlayer(this.players[this.id]);
            this.lerpOtherPlayers();
            ms -= dur;
            count += 1;
        }
        // Limit the time debt to 1 second. There's only so much we
        // can do to try and catch up.
        if (ms > 1000) ms = 1000;
        this.time_debt = ms;

        if (count > 3) console.log('updates', count);

    }
};

WebClient.prototype.updateDisplay = function (t) {
    this.drawGameState();
    this.updateid = window.requestAnimationFrame( this.updateDisplay.bind(this), this.canvas );
};
