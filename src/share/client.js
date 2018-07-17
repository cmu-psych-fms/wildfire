if (typeof Config === 'undefined') Config = require('./config').Config;
if (typeof GameEngineLib === 'undefined') GameEngineLib = require('./gameengine');
if (typeof GameEngine === 'undefined') GameEngine = GameEngineLib.GameEngine;


function Client (gnum, requestedId) {
    this.game_number = gnum;
    this.engine = new GameEngine(new Config());
    this.network = {serverUpdates: [],
                    latency: 0};
    this.requestedId = requestedId;
    this.id = null;
    this.keyEvents = [];
    this.movementRequests = [];
    this.movementRequestSeq = -1;
    this.gameStates = [];
    this.lastUpdateTime = null;
    this.leftOverDT = 0;
    this.status = 'disconnected';

    this.messageText = ["Impossible",
                        "I need help. Come to my position.",
                        "Split up to cover more ground.",
                        "Follow me.",
                        "10-4. In transit."];
}

Client.prototype = {};

Client.prototype.updateStatus = function (status, extra) {
    this.status = status;
};

Client.prototype.updatePoints = function (points) {
};

Client.prototype.cleanup = function () {
    this.engine.players = [];
    this.cancelUpdates();
};

Client.prototype.connect = function (socket) {
    this.network.socket = socket;
    this.network.socket.on('connect', this.onConnect.bind(this));
    this.network.socket.on('joined', this.onJoined.bind(this));

    this.network.socket.on('disconnect', this.onDisconnect.bind(this));
    this.network.socket.on('message', this.onMessage.bind(this));
    this.network.socket.on('serverupdate', this.onServerUpdate.bind(this));
    this.network.socket.on('join', this.onPlayerJoin.bind(this));
    this.network.socket.on('part', this.onPlayerPart.bind(this));
    this.network.socket.on('end', this.onEndGame.bind(this));
    this.network.socket.on('starting', this.onStarting.bind(this));
};

Client.prototype.cancelUpdates = function () {
    this.network.socket.close();
};

Client.prototype.keyNameToCode = function (name) {
    switch(name) {
    case 'fire':
        return GameEngineLib.KEY_SPACE;
    case 'left':
        return GameEngineLib.KEY_LEFT;
    case 'right':
        return GameEngineLib.KEY_RIGHT;
    case 'thrust':
        return GameEngineLib.KEY_UP;
    default:
        return -1;
    }
};

Client.prototype.pressKey = function (code) {
    this.network.hasNewInput = true;
    this.keyEvents.push([1,code]);
};

Client.prototype.releaseKey = function (code) {
    this.network.hasNewInput = true;
    this.keyEvents.push([0,code]);
};

Client.prototype.onConnect = function () {
    console.log('connect');
    this.network.socket.send('i' + JSON.stringify({id: this.requestedId, gnum: this.game_number}));
    // $('#status').html('<h1>Identifying Client ...</h1>');
    this.updateStatus('identification');
};

Client.prototype.onJoined = function (data) {
    console.log('joined', data);
    this.id = data.id;
    this.engine.startLocations = data.startLocations;
    for (let i=0; i<data.players.length; i++) {
        this.engine.addPlayer(data.players[i].id);
        this.engine.players[i].color = data.players[i].color;
    }
    this.engine.fortresses = new Array(data.fortresses.length);
    for (let i=0; i<data.fortresses.length; i++) {
        this.engine.fortresses[i] = {alive: data.fortresses[i][0],
                                     position: {x:data.fortresses[i][1],
                                                y:data.fortresses[i][2]},
                                     angle: data.fortresses[i][3],
                                     radius: data.fortresses[i][4]};
    }
    this.engine.asteroids = data.asteroids;
    this.engine.walls = data.walls;
    this.engine.spheres = data.spheres;

    // this.addWorldToScene();

    // $('#status').html('<h1>Waiting For Other Player ...</h1>');
    this.updateStatus('lobby');
};

Client.prototype.onStarting = function (data) {
    this.updateStatus('countdown', data.seconds);
};

Client.prototype.onDisconnect = function (data) {
    // We got disconnected
    console.log('disconnect', data);
    this.updateStatus('disconnected');
};


Client.prototype.sendPing = function () {
    var ts = new Date().getTime();
    this.network.socket.send('p'+ts.toString());
};

Client.prototype.handlePong = function (ts) {
    this.network.latency = new Date().getTime() - ts;
    console.log('latency is ', this.network.latency);
};

Client.prototype.onMessage = function (msg) {
    console.log('message', msg);
    var cmd = msg[0];
    var data = JSON.parse(msg.slice(1));
    switch (cmd) {
    case 'p':
        this.handlePong(data);
    };
};

Client.prototype.onPlayerJoin = function (data) {
    console.log('playerjoin', this);
    console.log('join', data);
    this.engine.addPlayer(data.id);
    this.updateStatus('countdown', 5);
    // this.addPlayerToScene(this.engine.getPlayer(data.id));
    // $('#status').html('<h1>Game will start in 5 seconds!</h1>');
};

Client.prototype.onPlayerPart = function (data) {
    console.log('part', data);
    // this.scene.remove(this.engine.getPlayer(data.id).mesh);
    this.engine.delPlayer(data.id);
};

Client.prototype.onEndGame = function (data) {
    this.updateStatus('finished');
    // console.log('end game');
    // exp.nextScreen();
};

Client.prototype.onServerUpdate = function (data) {
    this.network.serverUpdates.push(data);
    if (this.network.serverUpdates.length >= this.engine.config.serverUpdateBufferSize) {
        this.network.serverUpdates.splice(0,1);
    }
    if (this.status !== 'game') {
        console.log('status is', this.status, 'instead of game');
        this.updateStatus('game');
    }

    // if (!this.updateid) {
    //     $('#status_area').css('display', 'none');
    //     this.canvas3d.style.display = 'inline-block';
    //     this.requestUpdate();
    // }
};

Client.prototype.processKbdInput = function () {
    var p = this.engine.getPlayer(this.id);
    this.engine.processPlayerKeys(p, this.keyEvents);
    this.movementRequestSeq += 1;
    // console.log('master yah',
    //             this.engine.players[this.id].missileRequests,
    //             this.engine.players[this.id].messageRequests);
    var m = [this.movementRequestSeq,
             p.thrustFlag,
             p.turnFlag,
             p.missileRequests,
             p.messageRequests];
    var packet = 'k' + JSON.stringify(m);
    this.network.socket.send (packet);
    this.movementRequests.push(m);
    this.keyEvents = [];
    if (p.alive)
        this.engine.applyPlayerMovements(p);
};

Client.prototype.replayMovementRequests = function () {
    var p = this.engine.getPlayer(this.id);

    if (p.alive) {
        for (let i=0; i<this.movementRequests.length; i++) {
            p.thrustFlag = this.movementRequests[i][1];
            p.turnFlag = this.movementRequests[i][2];
            p.missileRequests = this.movementRequests[i][3];
            this.engine.applyPlayerMovements(p);
        }
    }
};

Client.prototype.lerpThing = function (thing) {
    if (thing.lerp.step >= thing.lerp.steps) return;

    thing.lerp.step += 1;
    thing.position.x += thing.lerp.dx;
    thing.position.y += thing.lerp.dy;
    thing.angle += thing.lerp.da;
};

Client.prototype.calcLerp = function (thing, start, end, ticks) {
    thing.lerp = { step: 0,
                   steps: ticks,
                   start: start,
                   end: end,
                   dx: (end.x - start.x)/ticks,
                   dy: (end.y - start.y)/ticks,
                   da: (end.angle - start.angle)/ticks}
};

Client.prototype.predictPlayer = function (p) {
    if (p.alive) {
        if (p.turnFlag === 'left') {
            p.angle -= p.config.turnRate;
        } else if (p.turnFlag === 'right') {
            p.angle += p.config.turnRate;
        }
        p.angle = GameEngineLib.stdAngle(p.angle);

        p.position.x += p.velocity.x;
        p.position.y += p.velocity.y;
    }
};

Client.prototype.predictMissile = function (m) {
    m.position.x += m.velocity.x;
    m.position.y += m.velocity.y;
};

Client.prototype.predictShell = function (s) {
    s.position.x += this.engine.config.shell.speed * Math.cos(GameEngineLib.deg2rad(s.angle));
    s.position.y += this.engine.config.shell.speed * Math.sin(GameEngineLib.deg2rad(s.angle));
};

Client.prototype.predictAsteroid = function (a) {
    a.position.x += a.velocity.x;
    a.position.y += a.velocity.y;
    a.angle = GameEngineLib.stdAngle(a.angle+a.angularVelocity);
};

Client.prototype.predictSphere = function (s) {
    s.position.x += s.velocity.x;
    s.position.y += s.velocity.y;
};

Client.prototype.predictiveStep = function () {
    for (let i=0; i<this.engine.missiles.length;i++)
        this.predictMissile(this.engine.missiles[i]);
    for (let i=0; i<this.engine.shells.length;i++)
        this.predictShell(this.engine.shells[i]);
    for (let i=0; i<this.engine.asteroids.length;i++)
        this.predictAsteroid(this.engine.asteroids[i]);
    for (let i=0; i<this.engine.spheres.length;i++)
        this.predictSphere(this.engine.spheres[i]);
};

Client.prototype.translateServerDefault = function (local, server) {
    local.position.x = server[1];
    local.position.y = server[2];
    local.angle = server[3];
};

Client.prototype.translateServerMissile = function (local, server) {
    local.position.x = server[1];
    local.position.y = server[2];
    local.velocity.x = server[3];
    local.velocity.y = server[4];
    local.angle = server[5];
};


Client.prototype.translateServerSphere = function (sphere, server) {
    sphere.position.x = server[1];
    sphere.position.y = server[2];
    sphere.velocity.x = server[3];
    sphere.velocity.y = server[4];
    sphere.target = server[5];
};

Client.prototype.processServerThingUpdate = function (thing, things, translateFn, processHookFn) {
    var match = -1;
    for (let j=0; j<things.length; j++) {
        if (things[j].id === thing[0]) {
            match = j;
            break;
        }
    }
    if (match >= 0) {
        translateFn(things[match], thing);
        things[match].active = true;
        processHookFn(things[match]);
        return things[match];
    } else {
        var o = {position: {x:0, y:0},
                 velocity: {x:0, y:0},
                 angle: 0,
                 id: thing[0],
                 active: true};
        translateFn(o, thing);
        things.push(o);
        processHookFn(o);
        return o;
    }
}

Client.prototype.processServerShellUpdate = function (shell) {
};

Client.prototype.processServerMissileUpdate = function (missile) {
};

Client.prototype.processServerSphereUpdate = function (sphere) {
};

Client.prototype.processServerShellUpdates = function (shells) {
    for (let i=this.engine.shells.length-1; i>=0; i--) {
        this.engine.shells[i].active = false;
    }
    for (let i=0; i<shells.length; i++) {
        this.processServerThingUpdate(shells[i], this.engine.shells,
                                      this.translateServerDefault.bind(this),
                                      this.processServerShellUpdate.bind(this));
    }
    for (let i=this.engine.shells.length-1; i>=0; i--) {
        if (!this.engine.shells[i].active) this.engine.shells.splice(i,1);
    }
};

Client.prototype.processServerMissileUpdates = function (missiles) {
    for (let i=this.engine.missiles.length-1; i>=0; i--) {
        this.engine.missiles[i].active = false;
    }
    for (let i=0; i<missiles.length; i++) {
        this.processServerThingUpdate(missiles[i], this.engine.missiles,
                                      this.translateServerMissile.bind(this),
                                      this.processServerMissileUpdate.bind(this));
    }
    for (let i=this.engine.missiles.length-1; i>=0; i--) {
        if (!this.engine.missiles[i].active) this.engine.missiles.splice(i,1);
    }
};

Client.prototype.processServerSphereUpdates = function (spheres) {
    for (let i=this.engine.spheres.length-1; i>=0; i--) {
        this.engine.spheres[i].active = false;
    }
    for (let i=0; i<spheres.length; i++) {
        var o = this.processServerThingUpdate(spheres[i], this.engine.spheres,
                                              this.translateServerSphere.bind(this),
                                              this.processServerSphereUpdate.bind(this));
    }
    for (let i=this.engine.spheres.length-1; i>=0; i--) {
        if (!this.engine.spheres[i].active) this.engine.spheres.splice(i,1);
    }
}

Client.prototype.processServerUpdates = function () {
    if (this.network.serverUpdates.length === 0) {
        for (let i=0; i<this.engine.players.length; i++) {
            if (this.engine.players[i].id !== this.id)
                this.predictPlayer(this.engine.players[i]);
        }
        this.predictiveStep();
    } else {
        // Scan all updates for messages
        for (let i=0; i<this.network.serverUpdates.length; i++) {
            var msg = this.network.serverUpdates[i].msg;
            for (let i=0; i<msg.length;i++) {
                this.engine.messages.push({player: this.engine.getPlayer(msg[i][0]), msg: msg[i][1], tick: this.engine.ticks});
                g_sounds[msg[i][1]].play();
            }
        }
        // We only need the last update
        var last = this.network.serverUpdates[this.network.serverUpdates.length-1];
        this.engine.points = last.points;
        this.updatePoints(this.engine.points);
        var players = last.p;
        for (let i=0;i<players.length;i++) {
            // console.log('update', k, players[k], this.engine.players[k])
            if (this.engine.players[i]) {
                this.engine.players[i].alive = players[i][0],
                this.engine.players[i].position.x = players[i][1];
                this.engine.players[i].position.y = players[i][2];
                this.engine.players[i].angle = players[i][3];
                this.engine.players[i].velocity.x = players[i][4];
                this.engine.players[i].velocity.y = players[i][5];
                this.engine.players[i].turnFlag = players[i][6];
            }
        }
        var fortresses = last.f;
        for (let i=0; i<this.engine.fortresses.length;i++) {
            this.engine.fortresses[i].alive = fortresses[i][0];
            this.engine.fortresses[i].position.x = fortresses[i][1];
            this.engine.fortresses[i].position.y = fortresses[i][2];
            this.engine.fortresses[i].angle = fortresses[i][3];
        }
        this.processServerShellUpdates(last.s);
        this.processServerMissileUpdates(last.m);
        this.processServerSphereUpdates(last.spheres);

        var asteroids = last.a;
        for (let i=0; i<this.engine.asteroids.length; i++) {
            this.engine.asteroids[i].position.x = asteroids[i][0];
            this.engine.asteroids[i].position.y = asteroids[i][1];
            this.engine.asteroids[i].angle = asteroids[i][2];
        }

        var lastMovementRequest = last.lmr;
        for (let i=0; i<this.movementRequests.length; i++) {
            if (lastMovementRequest === this.movementRequests[i][0]) {
                this.movementRequests.splice(0,i+1);
                break;
            }
        }
        this.replayMovementRequests();
        // This should reduce rubber-banding when latency is high
        for (let i=0; i<this.movementRequests.length; i++)
            this.predictiveStep();

        this.network.serverUpdates.length = 0;
    }
};

Client.prototype.updateMessages = function () {
    for (let i=this.engine.messages.length-1; i>=0; i--) {
        if (this.engine.messages[i].tick + this.engine.config.message.duration <= this.engine.ticks ) {
            this.engine.messages.splice(i, 1);
        }
    }
};

Client.prototype.saveState = function () {
};

Client.prototype.update = function (t) {
    this.dt = this.lastUpdateTime ? (t - this.lastUpdateTime) : 1000/60.0;
    this.lastUpdateTime = t;
    // If the frame rate drops below 60fps, we need to catch up by
    // doing doing multiple game ticks.
    var ms = this.leftOverDT + this.dt;
    var ticks = Math.floor(ms / (1000/60.0));
    this.leftOverDT = ms - ticks * 1000/60.0;
    // console.log(this.lastUpdateTime, this.dt, ms, ticks, this.leftOverDT);
    for (let i=0; i<ticks; i++) {
        // This is zero'd out in step_one_tick but the client doesn't call
        // that so we need to do this manually.
        this.engine.events.length = 0;
        this.engine.ticks += 1;
        this.processKbdInput();
        this.processServerUpdates();
        this.saveState();
    }
};

(function(exports) {
    exports.Client = Client;
})(typeof exports === 'undefined' ? this['client']={}:exports);
