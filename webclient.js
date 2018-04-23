//Code below is from Three.js, and sourced from links below

    // http://paulirish.com/2011/requestanimationframe-for-smart-animating/
    // http://my.opera.com/emoller/blog/2011/12/20/requestanimationframe-for-smart-er-animating

    // requestAnimationFrame polyfill by Erik Möller
    // fixes from Paul Irish and Tino Zijdel

( function () {
    var frame_time = 60/1000;
    var lastTime = 0;
    var vendors = [ 'ms', 'moz', 'webkit', 'o' ];

    for ( var x = 0; x < vendors.length && !window.requestAnimationFrame; ++ x ) {
        window.requestAnimationFrame = window[ vendors[ x ] + 'RequestAnimationFrame' ];
        window.cancelAnimationFrame = window[ vendors[ x ] + 'CancelAnimationFrame' ] || window[ vendors[ x ] + 'CancelRequestAnimationFrame' ];
    }

    if ( !window.requestAnimationFrame ) {
        window.requestAnimationFrame = function ( callback, element ) {
            var currTime = Date.now(), timeToCall = Math.max( 0, frame_time - ( currTime - lastTime ) );
            var id = window.setTimeout( function() { callback( currTime + timeToCall ); }, timeToCall );
            lastTime = currTime + timeToCall;
            return id;
        };
    }

    if ( !window.cancelAnimationFrame ) {
        window.cancelAnimationFrame = function ( id ) { clearTimeout( id ); };
    }

}() );

function WebClient (engine) {
    this.engine = new GameEngine(new Config());
    this.network = {serverUpdates: [],
                    latency: 0};
    this.id = null;

    this.ticks = 0;

    this.keyEvents = [];

    this.movementRequests = [];
    this.curMovementRequestSeq = 0;

    this.mapChangePredictions = [];

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
}

WebClient.prototype = {};

WebClient.prototype.connect = function () {
    this.network.socket = io.connect();
    this.network.socket.on('connected', this.onConnect.bind(this));

    this.network.socket.on('disconnect', this.onDisconnect.bind(this));
    this.network.socket.on('message', this.onMessage.bind(this));
    this.network.socket.on('serverupdate', this.onServerUpdate.bind(this));
    this.network.socket.on('join', this.onPlayerJoin.bind(this));
    this.network.socket.on('part', this.onPlayerPart.bind(this));
    this.network.socket.on('reset', this.onReset.bind(this));
};

WebClient.prototype.begin = function () {
    this.connect();
    document.addEventListener('keydown', this.onKeyDown.bind(this));
    document.addEventListener('keyup', this.onKeyUp.bind(this));

    this.canvas = document.getElementById('gamecanvas');
    // this.canvas.width = 710;
    // this.canvas.height = 630;
    this.canvas.width = 600;
    this.canvas.height = 500;

    this.ctx = this.canvas.getContext('2d');
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
    else
        return undefined;
};

WebClient.prototype.cancelUpdates = function () {
    console.log("cancel animation");
    window.cancelAnimationFrame(this.updateid);
    this.stopGameTickTimer();
    this.network.socket.close();
};

WebClient.prototype.onKeyDown = function (ev) {
    if (ev.which === 27) this.cancelUpdates();
    var k = this.decodeKeyCode(ev.which);
    if (k) {
        this.network.hasNewInput = true;
        this.keyEvents.push([1, k]);
        ev.preventDefault();
        ev.stopPropagation();
    }
};

WebClient.prototype.onKeyUp = function (ev) {
    var k = this.decodeKeyCode(ev.which);
    if (k) {
        this.network.hasNewInput = true;
        this.keyEvents.push([0, k]);
        ev.preventDefault();
        ev.stopPropagation();
    }
};

WebClient.prototype.onReset = function (data) {
    for (let k in data.players) {
        this.engine.players[k].alive = true;
        this.engine.players[k].position.x = data.players[k][0];
        this.engine.players[k].position.y = data.players[k][1];
        this.engine.players[k].angle = data.players[k][2];
    }
    this.engine.map = data.map;
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

WebClient.prototype.onConnect = function (data) {
    // console.log('connect', data);

    this.id = data.id;
    for (let k in data.players) {
        this.initPlayer(k);
        var p = this.players[k];

        console.log('before', p);
        p.alive = data.players[k][0];
        p.position.x = data.players[k][1];
        p.position.y = data.players[k][2];
        p.angle = data.players[k][3];
        p.speed = data.players[k][4];
        p.water = data.players[k][5];

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
    this.engine.map = data.map;

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
    this.network.socket.send('r0');
}

WebClient.prototype.sendPing = function () {
    var ts = new Date().getTime();
    this.network.socket.send('p'+ts.toString());
};

WebClient.prototype.handlePong = function (ts) {
    this.network.latency = new Date().getTime() - ts;
    console.log('latency is ', this.network.latency);
};

WebClient.prototype.onMessage = function (msg) {
    console.log('message', msg);
    var cmd = msg[0];
    var data = JSON.parse(msg.slice(1));
    switch (cmd) {
    case 'p':
        this.handlePong(data);
    };
};

WebClient.prototype.onPlayerJoin = function (data) {
    console.log('join', data);
    this.initPlayer(data.id);
};

WebClient.prototype.onPlayerPart = function (data) {
    console.log('part', data);
    delete this.players[data.id];
};


WebClient.prototype.onServerUpdate = function (data) {
    this.network.serverUpdates.push(data);
    if (this.network.serverUpdates.length >= this.engine.config.serverUpdateBufferSize) {
        this.network.serverUpdates.splice(0,1);
    }
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

        // console.log(ofs, dist);
        if (ofs > dist) {
            this.setPlayerCurrent(p);
        } else {
            p.position.x = p.last.position.x + (p.latest.position.x - p.last.position.x) * ofs / dist;
            p.position.y = p.last.position.y + (p.latest.position.y - p.last.position.y) * ofs / dist;
            p.angle = stdAngle(p.last.angle + angle_diff(p.latest.angle,p.last.angle) * ofs / dist);
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
        var k = [ this.curMovementRequestSeq, p.turnFlag, p.thrustFlag, p.dumpFlag ]
        // console.log(k);
        this.movementRequests.push(k);
        var packet = 'k' + JSON.stringify(k);
        this.network.socket.send (packet);
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

WebClient.prototype.mapAtUnsafe = function (x, y) {
    var r = this.engine.map[y*this.engine.config.mapSize+x];
    for (let i=0; i<this.mapChangePredictions.length; i++) {
        if (this.mapChangePredictions[i][0] === x &&
            this.mapChangePredictions[i][1] === y)
            r = this.mapChangePredictions[i][2];
    }
    return r;
};

WebClient.prototype.addMapChange = function (x, y, r) {
    if (x>=0 && x < this.engine.config.mapSize &&
        y>=0 && y < this.engine.config.mapSize &&
        this.mapAtUnsafe(x,y) !== r) {
        this.mapChangePredictions.push([x, y, r]);
    }
};

WebClient.prototype.stepPlayer = function (p) {
    if (p.alive) {
        this.engine.playerApplyMovement(p);

        if (p.dumpFlag) {
            var mx = Math.round(p.position.x/this.engine.config.mapCellSize),
                my = Math.round(p.position.y/this.engine.config.mapCellSize),
                m = this.engine.mapAt(mx, my);
            if (m !== MAP_WATER && m !== MAP_ROCK) {
                this.addMapChange(mx, my, MAP_RETARDANT);
            }
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
                this.engine.map[mapUpdates[i][0]] = mapUpdates[i][1];
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
            if (this.movementRequests[i][0] === this.currentServerUpdate.lk.seq) {
                this.movementRequests.splice(0,i+1);
                break;
            }
        }
        this.mapChangePredictions.length = 0;

        this.players[this.id].backup_x = this.players[this.id].position.x;
        this.players[this.id].backup_y = this.players[this.id].position.y;
        this.players[this.id].backup_a = this.players[this.id].angle;
        this.players[this.id].backup_speed = this.players[this.id].speed;

        this.setPlayerCurrent(this.players[this.id]);
        // replay our key events from the last authoritative game state.
        this.replayPlayerKeys();

        this.network.serverUpdates.length = 0;
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

WebClient.prototype.drawGameState = function () {
    this.ctx.save();
    this.ctx.clearRect(0,0,this.canvas.width,this.canvas.height);
    this.ctx.translate(-this.players[this.id].position.x+this.canvas.width/2,
                       -this.players[this.id].position.y+this.canvas.height/2);
    // this.ctx.strokeStyle = '#003300';
    var maxx = this.engine.config.mapSize * this.engine.config.mapCellSize;
    var maxy = this.engine.config.mapSize * this.engine.config.mapCellSize;
    // for (let x=0; x<this.engine.config.mapSize; x++) {
    //     this.ctx.moveTo(x*this.engine.config.mapCellSize, 0);
    //     this.ctx.lineTo(x*this.engine.config.mapCellSize, maxy);
    // }
    // for (let y=0; y<this.engine.config.mapSize; y++) {
    //     this.ctx.moveTo(0, y*this.engine.config.mapCellSize);
    //     this.ctx.lineTo(maxx, y*this.engine.config.mapCellSize);
    // }
    // this.ctx.stroke();
    this.ctx.lineWidth = 4;
    this.ctx.strokeStyle = '#00AA00';
    this.ctx.strokeRect(-2, -2, maxx+4, maxy+4);
    this.ctx.lineWidth = 1;

    var startx = Math.round((this.players[this.id].position.x - this.canvas.width/2)/this.engine.config.mapCellSize);
    var starty = Math.round((this.players[this.id].position.y - this.canvas.height/2)/this.engine.config.mapCellSize);
    var endx = startx+Math.round(this.canvas.width/this.engine.config.mapCellSize);
    var endy = starty+Math.round(this.canvas.height/this.engine.config.mapCellSize);
    if (startx < 0) startx = 0;
    if (starty < 0) starty = 0;
    if (endx >= this.engine.config.mapSize) endx = this.engine.config.mapSize-1;
    if (endy >= this.engine.config.mapSize) endy = this.engine.config.mapSize-1;

    for (let y=starty; y<=endy; y++) {
        for (let x=startx; x<=endx; x++) {
            var m = this.map_images[this.engine.map[y*this.engine.config.mapSize+x]];
            if (!m) console.log(this.engine.map[y*this.engine.config.mapSize+x]);
            this.ctx.drawImage(m,
                               x * this.engine.config.mapCellSize,
                               y * this.engine.config.mapCellSize);
        }
    }
    for (let i=0; i<this.mapChangePredictions.length; i++) {
        if (this.mapChangePredictions[i][0] >= startx &&
            this.mapChangePredictions[i][0] <= endx &&
            this.mapChangePredictions[i][1] >= starty &&
            this.mapChangePredictions[i][1] <= endy) {
            var m = this.map_images[this.mapChangePredictions[i][2]];
            this.ctx.drawImage(m,
                               this.mapChangePredictions[i][0] * this.engine.config.mapCellSize,
                               this.mapChangePredictions[i][1] * this.engine.config.mapCellSize);
        }
    }


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

    this.ctx.restore();
};

WebClient.prototype.gameLogicUpdate = function () {
    var t = new Date().getTime();
    this.dt = this.lastUpdateTime ? (t - this.lastUpdateTime) : 1000/60.0;
    this.lastUpdateTime = t;
    this.ticks += 1;

    this.processServerUpdates();
    this.processKbdInput();

    this.placeMovementRequest();
    this.stepPlayer(this.players[this.id]);

    this.lerpOtherPlayers();
};

WebClient.prototype.updateDisplay = function (t) {
    this.drawGameState();
    this.updateid = window.requestAnimationFrame( this.updateDisplay.bind(this), this.canvas );
};
