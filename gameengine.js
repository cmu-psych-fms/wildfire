var KEY_LEFT = 1;
var KEY_RIGHT = 2;
var KEY_UP = 3;
var KEY_DOWN = 4;
var KEY_SPACE = 5;

function deg2rad(deg) {
    return deg * Math.PI / 180;
}

function mod(x,y) {
    return ((x%y)+y)%y;
}

function stdAngle(a) {
    a = mod(a, 360);
    if (a >= 180) a -= 360;
    else if (a <= -180) a += 360;
    return a;
}


function GameEngine(config) {
    this.config = config;
    this.players = {};
}

GameEngine.prototype = {};

GameEngine.prototype.createMap = function () {
    this.map = Array(this.config.mapSize * this.config.mapSize);
};

GameEngine.prototype.stepOneTick = function () {
    this.updatePlayers();
    // this.updateEntities();
    // this.handleCollisions();
};

GameEngine.prototype.processPlayerKeys = function (p, keys) {
    for (var i=0; i<keys.length; i++) {
        console.log(keys[i][0] ? 'pressed':'released', keys[i][1]);
        if (keys[i][0] === 1) {
            if (keys[i][1] === KEY_LEFT) p.turnFlag = 'left';
            else if (keys[i][1] === KEY_RIGHT) p.turnFlag = 'right';
            else if (keys[i][1] === KEY_UP) p.thrustFlag = true;
        } else {
            if (keys[i][1] === KEY_LEFT || keys[i][1] === KEY_RIGHT) p.turnFlag = null;
            else if (keys[i][1] === KEY_UP) p.thrustFlag = false;
        }
    }
};

GameEngine.prototype.updatePlayer = function (p) {
    if (p.turnFlag === 'left') {
        p.angle -= p.config.turnRate;
        p.angle = stdAngle(p.angle);
    } else if (p.turnFlag === 'right') {
        p.angle += p.config.turnRate;
        p.angle = stdAngle(p.angle);
    }
    if (p.thrustFlag) {
        p.velocity.x += p.config.acceleration * Math.cos(deg2rad(p.angle));
        p.velocity.y += p.config.acceleration * Math.sin(deg2rad(p.angle));
    }
    if (p.velocity.x > p.config.maxSpeed) p.velocity.x = p.config.maxSpeed;
    if (p.velocity.x < -p.config.maxSpeed) p.velocity.x = -p.config.maxSpeed;
    if (p.velocity.y > p.config.maxSpeed) p.velocity.y = p.config.maxSpeed;
    if (p.velocity.y < -p.config.maxSpeed) p.velocity.y = -p.config.maxSpeed;

    p.position.x += p.velocity.x;
    p.position.y += p.velocity.y;

    // console.log(p);
};

GameEngine.prototype.updatePlayers = function () {
    for (let id in this.players) {
        this.updatePlayer(this.players[id]);
    }
};

GameEngine.prototype.addPlayer = function (id) {
    this.players[id] = {id: id,
                        angle: this.config.player.startAngle,
                        position: {x: this.config.player.startPosition.x,
                                   y: this.config.player.startPosition.y},
                        velocity: {x: this.config.player.startVelocity.x,
                                   y: this.config.player.startVelocity.y},
                        config: this.config.player
                       };
};

GameEngine.prototype.delPlayer = function (id) {
    delete this.players[id];
};

exports.GameEngine = GameEngine;
