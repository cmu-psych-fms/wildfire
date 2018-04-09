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
};

WebClient.prototype.begin = function () {
    // this.connect();
    document.addEventListener('keydown', this.onKeyDown.bind(this));
    document.addEventListener('keyup', this.onKeyUp.bind(this));

    this.canvas = document.getElementById('gamecanvas');
    this.canvas.width = 710;
    this.canvas.height = 630;
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
    }
};

WebClient.prototype.onKeyUp = function (ev) {
    var k = this.decodeKeyCode(ev.which);
    if (k) {
        this.network.hasNewInput = true;
        this.keyEvents.push([0, k]);
    }
};

WebClient.prototype.onConnect = function (data) {
    console.log('connect', data);
    this.id = data.id;
    for (let k in data.snapshot) {
        this.engine.addPlayer(k);
        this.engine.players[k].position.x = data.snapshot[k][0];
        this.engine.players[k].position.y = data.snapshot[k][1];
        this.engine.players[k].angle = data.snapshot[k][2];
    }

    game.update( new Date().getTime() );
};

WebClient.prototype.onDisconnect = function (data) {
    console.log('disconnect', data);
    // We got disconnected
    this.network.state = 'disconnected';
    this.cancelUpdates();
};


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

WebClient.prototype.processServerUpdates = function () {
    for (let i=0; i<this.network.serverUpdates.length; i++) {
        var players = this.network.serverUpdates[i];
        for (let k in players) {
            // console.log('update', k, players[k], this.engine.players[k])
            if (this.engine.players[k]) {
                this.engine.players[k].position.x = players[k][0];
                this.engine.players[k].position.y = players[k][1];
                this.engine.players[k].angle = players[k][2];
            }
        }
    }
    this.network.serverUpdates.length = 0;
};

WebClient.prototype.drawGameState = function () {
    this.ctx.clearRect(0,0,this.canvas.width,this.canvas.height);
    this.ctx.strokeStyle = '#009900';
    for (let x=0; x<this.engine.config.mapSize; x++)
        for (let y=0; y<this.engine.config.mapSize; y++) {
            this.ctx.strokeRect (x*20, y*20, 20, 20);
        }


    for (let id in this.engine.players) {
        shipWireframe.draw (this.ctx,
                            this.engine.players[id].position.x,
                            this.engine.players[id].position.y,
                            this.engine.players[id].angle);
    }
};

WebClient.prototype.update = function (t) {
    this.dt = this.lastUpdateTime ? (t - this.lastUpdateTime) : 1000/60.0;
    this.lastUpdateTime = t;

    this.processKbdInput();
    this.processServerUpdates();
    this.drawGameState();

    this.updateid = window.requestAnimationFrame( this.update.bind(this), this.canvas );
};
