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
    this.keyEvents = [];

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

WebClient.prototype.onConnect = function (data) {
    // console.log('connect', data);
    this.id = data.id;
    for (let k in data.players) {
        this.engine.addPlayer(k);
        console.log('before', this.engine.players[k]);
        this.engine.players[k].alive = data.players[k][0];
        this.engine.players[k].position.x = data.players[k][1];
        this.engine.players[k].position.y = data.players[k][2];
        this.engine.players[k].angle = data.players[k][3];
        this.engine.players[k].speed = data.players[k][4];

        // console.log(data.players[k][1], data.players[k][2], data.players[k][3]);

        // console.log(this.engine.players[k]);
    }
    this.engine.map = data.map;
    // this.engine.fortresses = new Array(data.fortresses.length);
    // for (let i=0; i<data.fortresses.length; i++) {
    //     this.engine.fortresses[i] = {alive: data.fortresses[i][0],
    //                                  position: {x:data.fortresses[i][1],
    //                                             y:data.fortresses[i][2]},
    //                                  angle: data.fortresses[i][3],
    //                                  radius: data.fortresses[i][4]};
    // }
    // this.engine.asteroids = data.asteroids;

    game.update( new Date().getTime() );

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
    this.engine.addPlayer(data.id);
};

WebClient.prototype.onPlayerPart = function (data) {
    console.log('part', data);
    this.engine.delPlayer(data.id);
};


WebClient.prototype.onServerUpdate = function (data) {
    this.network.serverUpdates.push(data);
    if (this.network.serverUpdates.length >= this.engine.config.serverUpdateBufferSize) {
        this.network.serverUpdates.splice(0,1);
    }
};

WebClient.prototype.processKbdInput = function () {
    // for (let i=0; i<this.keyEvents.length; i++) {
    // }
    if (this.network.hasNewInput) {
        var packet = 'k' + JSON.stringify(this.keyEvents);
        this.network.socket.send (packet);
        this.keyEvents = [];
        this.network.hasNewInput = false;
    }
};

WebClient.prototype.predictPlayerMovement = function (p) {
    if (p.alive) {
        if (p.turnFlag === 'left') p.angle -= this.engine.config.player.turnRate;
        else if (p.turnFlag === 'right') p.angle += this.engine.config.player.turnRate;
        p.angle = stdAngle(p.angle);

        p.position.x += p.speed * Math.cos(deg2rad(p.angle));
        p.position.y += p.speed * Math.sin(deg2rad(p.angle));

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

WebClient.prototype.processServerUpdates = function () {
    if (this.network.serverUpdates.length === 0) {
        for (let k in this.engine.players) {
            var p = this.engine.players[k];
            if (p.lerp)
                this.interpolatePlayer(p);
            else
                this.predictPlayerMovement(p);
        }
    } else {
        // Need to run through all updates for map changes
        for (let i=0; i<this.network.serverUpdates.length; i++) {
            var mapUpdates = this.network.serverUpdates[i].m;
            for (let i=0; i<mapUpdates.length;i++) {
                this.engine.map[mapUpdates[i][0]] = mapUpdates[i][1];
            }
        }
        // Keep track of game ticks on client and server. When we get
        // a server tick. Predict it up to the client's tick then use
        // interpolation if necessary.
        //
        // This fixes the jerking back in time whenever we get a laggy
        // packet from the server.

        // But we only need the latest update for player positions
        var p_u = this.network.serverUpdates[this.network.serverUpdates.length-1].p;
        for (let k in p_u) {
            var p = this.engine.players[k];
            if (p) {
                p.alive = p_u[k][0],
                p.speed = p_u[k][4];
                p.turnFlag = p_u[k][5];
                if (p.alive &&
                    (Math.abs(p.position.x - p_u[k][1]) > this.engine.config.network.maxDist ||
                     Math.abs(p.position.y - p_u[k][2]) > this.engine.config.network.maxDist ||
                     Math.abs(angle_diff(p.angle - p_u[k][3])) > this.engine.config.network.maxAngle)) {
                    p.lerp = { steps: 4,
                               step: 0,
                               lastKnown: {x:p.position.x, y:p.position.y, a:p.angle},
                               target: {x:p_u[k][1], y:p_u[k][2], a:p_u[k][3]}}
                } else {
                    p.position.x = p_u[k][1];
                    p.position.y = p_u[k][2];
                    p.angle = p_u[k][3];
                }
            }
        }
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
    this.ctx.translate(-this.engine.players[this.id].position.x+this.canvas.width/2,
                       -this.engine.players[this.id].position.y+this.canvas.height/2);
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

    var startx = Math.round((this.engine.players[this.id].position.x - this.canvas.width/2)/this.engine.config.mapCellSize);
    var starty = Math.round((this.engine.players[this.id].position.y - this.canvas.height/2)/this.engine.config.mapCellSize);
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

    for (let id in this.engine.players) {
        if (this.engine.players[id].alive) {
            this.ctx.save();
            this.ctx.translate(this.engine.players[id].position.x,
                               this.engine.players[id].position.y);
            this.ctx.rotate(deg2rad(this.engine.players[id].angle));
            this.ctx.drawImage(g_images['plane'], -g_images['plane'].width/2-10, -g_images['plane'].height/2);
            this.ctx.restore();
        }
            // shipWireframe.draw (this.ctx,
            //                     this.engine.players[id].position.x,
            //                     this.engine.players[id].position.y,
            //                     this.engine.players[id].angle);
        else
            this.drawExplosion(this.ctx,
                               this.engine.players[id].position.x,
                               this.engine.players[id].position.y);
    }

    this.ctx.restore();
};

WebClient.prototype.update = function (t) {
    this.dt = this.lastUpdateTime ? (t - this.lastUpdateTime) : 1000/60.0;
    this.lastUpdateTime = t;

    this.processKbdInput();
    this.processServerUpdates();
    this.drawGameState();

    this.updateid = window.requestAnimationFrame( this.update.bind(this), this.canvas );
};
