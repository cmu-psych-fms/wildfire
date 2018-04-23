var KEY_LEFT = 1;
var KEY_RIGHT = 2;
var KEY_UP = 3;
var KEY_DOWN = 4;
var KEY_SPACE = 5;

var MAP_FIRE = 1;
var MAP_ASH = 2;
var MAP_RETARDANT = 3;
var MAP_WATER = 4;
var MAP_TREE = 5;
var MAP_HOUSE = 6;
var MAP_ROCK = 7;
var MAP_GRASS = 8;
var MAP_VROAD = 9;
var MAP_HROAD = 10;

var PLAYER_SERVER_UPDATE_KEYS = ['alive', ['position', 'x'], ['position', 'y'], 'speed', 'turnFlag'];

function angle_diff(angle1, angle2) {
    // It finally works--don't touch it!
    if (angle1 < 90) {
        if (angle2 > angle1+180) return 360-angle2 + angle1;
        else return angle1 - angle2;
    } else if (angle1 < 180) {
        if (angle2 > angle1+180) return 360-angle2 + angle1;
        else return angle1 - angle2;
    } else if (angle1 < 270) {
        if (angle2 < angle1-180) return -(angle2 + 360-angle1);
        else return angle1 - angle2;
    } else {
        if (angle2 < angle1-180) return -(angle2 + 360-angle1);
        else return angle1 - angle2;
    }
}

function rotate_translate (o_x, o_y, x, y, angle) {
    // FIXME: we shouldn't need to tweak the angle
    var a = deg2rad(-angle+90);
    var s = Math.cos(a);
    var c = Math.sin(a);
    return {x: x * c - y * s + o_x,
            y: y * c + x * s + o_y};
}


function angleTo(p1, p2) {
    var a = Math.atan2(p2.y-p1.y, p2.x-p1.x);
    return stdAngle(rad2deg(a));
}

function distance(p1, p2) {
    return Math.sqrt(Math.pow(p1.x-p2.x,2)+Math.pow(p1.y-p2.y,2));
}

function deg2rad(deg) {
    return deg * Math.PI / 180;
}

function rad2deg(rad) {
    return rad / Math.PI * 180;
}

function mod(x,y) {
    return ((x%y)+y)%y;
}

function stdAngle(a) {
    a = mod(a,360)
    if (a < 0) a += 360;
    return a;
}


function V(x,y) {
    return {x:x,y:y};
}

function GameEngine(config) {
    this.config = config;
    this.players = {};
    this.sparks = [];
    this.map = new Array(this.config.mapSize * this.config.mapSize);
    this.mapUpdates = [];
    this.wind = {x:0, y:0};
    this.ticks = 0;
}

GameEngine.prototype = {};

GameEngine.prototype.mapAt = function (x,y) {
    if (x>=0 && x < this.config.mapSize &&
        y>=0 && y < this.config.mapSize)
        return this.map[y*this.config.mapSize+x];
};

GameEngine.prototype.mapSet = function (x,y,r) {
    if (x>=0 && x < this.config.mapSize &&
        y>=0 && y < this.config.mapSize) {
        var idx = y*this.config.mapSize+x;
        if (this.map[idx] !== r) {
            this.map[idx] = r;
            this.mapUpdates.push(idx);
        }
    }
};

GameEngine.prototype.createMap = function () {
    for (let y=0; i<this.config.mapSize; i++) {
        for (let x=0; i<this.config.mapSize; i++) {
            this.map[y*this.config.mapSize + x] = 0;
        }
    }
};

GameEngine.prototype.createMap = function () {
    this.map = new Array(this.config.mapSize * this.config.mapSize);
};

GameEngine.prototype.startSomeFires = function () {
    var n = this.config.startFires;
    for (let i=0; i<n; i++) {
        var x = Math.round(Math.random()*this.config.mapSize);
        var y = Math.round(Math.random()*this.config.mapSize);
        this.ignite(x,y);
        this.ignite(x+1,y);
        this.ignite(x,y+1);
        this.ignite(x-1,y);
        this.ignite(x,y-1);
    }
};

GameEngine.prototype.stepOneTick = function () {
    this.ticks += 1;
    this.updatePlayers();
    this.updateSparks();
    this.updateFires();
};

GameEngine.prototype.ignite = function (x, y) {
    var m = this.mapAt(x,y);
    if (m !== MAP_ROCK && m !== MAP_HROAD && m !== MAP_VROAD && m !== MAP_WATER && m !== MAP_ASH && m !== MAP_RETARDANT)
        this.mapSet(x,y, MAP_FIRE);
}

GameEngine.prototype.extinguish = function (x, y) {
    var m = this.mapAt(x,y);
    if (m === MAP_FIRE) {
        this.mapSet(x,y, MAP_ASH);
        return true;
    }
}

// GameEngine.prototype.addSmoke = function (x, y) {
//     var s = {position: {x:x*this.config.mapCellSize,
//                                 y:y*this.config.mapCellSize},
//              velocity: {x: 0.1, y: Math.random()*0.1-0.05},
//              tick: this.ticks};
//     this.smoke.push(s);
//     this.newSmoke.push(s);
// }

GameEngine.prototype.updateFires = function () {
    var n = 2;
    for (let i=0; i<n; i++) {
        var x = Math.round(Math.random()*this.config.mapSize);
        var y = Math.round(Math.random()*this.config.mapSize);
        var m = this.mapAt(x,y);
        if (m === MAP_FIRE) {
            this.mapSet(x,y, MAP_ASH);
            this.ignite(x+1,y);
            this.ignite(x,y+1);
            this.ignite(x-1,y);
            this.ignite(x,y-1);
        } if (m === MAP_RETARDANT) {
            var r1 = this.extinguish(x+1,y);
            var r2 = this.extinguish(x,y+1);
            var r3 = this.extinguish(x-1,y);
            var r4 = this.extinguish(x,y-1);
            if (r1||r2||r3||r4) this.mapSet(x,y,MAP_GRASS);
        }
    }
    // for (let i=0; i<10; i++) {
    //     var x = Math.round(Math.random()*this.config.mapSize);
    //     var y = Math.round(Math.random()*this.config.mapSize);
    //     var m = this.mapAt(x,y);
    //     if (m === MAP_FIRE) {
    //         this.addSmoke(x,y);
    //         // this.smoke.push({position: {x:x*this.config.mapCellSize,
    //         //                             y:y*this.config.mapCellSize},
    //         //                  velocity: {x: 0.1, y: Math.random()*0.2-0.1}});
    //     }
    // }
};

GameEngine.prototype.updateSparks = function () {

};

// GameEngine.prototype.updateSmoke = function () {
//     for (let i=this.smoke.length-1; i>=0; i--) {
//         if (this.ticks >= this.smoke[i].tick+this.config.smoke.duration)
//             this.smoke.splice(i,1);
//         else {
//             this.smoke[i].position.x += this.smoke[i].velocity.x;
//             this.smoke[i].position.y += this.smoke[i].velocity.y;
//         }
//         for (let k in this.players) {
//             if (distance(this.smoke[i].position, this.players[k].position) < 30) {
//                 this.players[k].health -= 1;
//             }
//         }
//     }
// };

GameEngine.prototype.processPlayerKeysHelper = function (p, keys) {
    for (var i=0; i<keys.length; i++) {
        // console.log(keys[i][0] ? 'pressed':'released', keys[i][1]);
        if (keys[i][0] === 1) {
            if (keys[i][1] === KEY_LEFT) p.turnFlag = 'l';
            else if (keys[i][1] === KEY_RIGHT) p.turnFlag = 'r';
            else if (keys[i][1] === KEY_UP) p.thrustFlag = 'f';
            else if (keys[i][1] === KEY_DOWN) p.thrustFlag = 's';
            else if (keys[i][1] === KEY_SPACE) p.dumpFlag = 1;
        } else {
            if (keys[i][1] === KEY_LEFT || keys[i][1] === KEY_RIGHT) p.turnFlag = 0;
            else if (keys[i][1] === KEY_UP) p.thrustFlag = 0;
            else if (keys[i][1] === KEY_DOWN) p.thrustFlag = 0;
            else if (keys[i][1] === KEY_SPACE) p.dumpFlag = 0;
        }
    }
}

GameEngine.prototype.processPlayerKeys = function (p, data) {
    // p.lastKey.tick = this.ticks+1;
    p.outstandingMovementRequests.push(data);
};

GameEngine.prototype.resetPlayer = function (p) {
    p.alive = true;
    p.position.x = this.config.player.startPosition.x;
    p.position.y = this.config.player.startPosition.y;
    p.speed = 0;
    p.angle = this.config.player.startAngle;
    p.health = this.config.player.health;
};

GameEngine.prototype.playerApplyMovement = function (p) {
    // console.log(p.lastKey.seq, p.turnFlag);
    if (p.turnFlag === 'l') {
        p.angle -= p.config.turnRate;
        p.angle = stdAngle(p.angle);
    } else if (p.turnFlag === 'r') {
        p.angle += p.config.turnRate;
        p.angle = stdAngle(p.angle);
    }
    if (p.thrustFlag === 'f') {
        p.speed += 0.1;
        if (p.speed > this.config.player.maxSpeed) p.speed = this.config.player.maxSpeed;
    } else if (p.thrustFlag === 's') {
        p.speed -= 0.1;
        if (p.speed < 0) p.speed = 0;
    }

    p.position.x += p.speed * Math.cos(deg2rad(p.angle));
    p.position.y += p.speed * Math.sin(deg2rad(p.angle));
};

GameEngine.prototype.playerApplyDump = function (p) {
    if (p.dumpFlag) {
        var mx = Math.round(p.position.x/this.config.mapCellSize),
            my = Math.round(p.position.y/this.config.mapCellSize),
            m = this.mapAt(mx, my);
        if (m !== MAP_WATER && m !== MAP_ROCK) {
            this.mapSet(mx, my, MAP_RETARDANT);
        }
    }
};

GameEngine.prototype.processOutstandingPlayerMovements = function (p) {
    for (let i=0; i<p.outstandingMovementRequests.length; i++) {
        p.lastKey.seq = p.outstandingMovementRequests[i][0];
        p.turnFlag = p.outstandingMovementRequests[i][1];
        p.thrustFlag = p.outstandingMovementRequests[i][2];
        p.dumpFlag = p.outstandingMovementRequests[i][3];
        this.playerApplyMovement(p);
        this.playerApplyDump(p);
    }
    p.outstandingMovementRequests.length = 0;
};

GameEngine.prototype.updatePlayer = function (p) {
    if (p.alive) {

        this.processOutstandingPlayerMovements(p);

        if (p.health < 0) {
            this.killPlayer(p);
        }

        // if (p.position.x < 0 ||
        //     p.position.x > this.config.mapSize * this.config.mapCellSize ||
        //     p.position.y < 0 ||
        //     p.position.y > this.config.mapSize * this.config.mapCellSize) {
        //     this.killPlayer(p);
        // }
    } else {
        p.spawnTimer += 1;
        if (p.spawnTimer >= this.config.player.deathTimer) {
            this.resetPlayer(p);
        }
    }
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
                        speed: 0,
                        config: this.config.player,
                        turnFlag: 0,
                        thrustFlag: 0,
                        water: 100,
                        spawnTimer: 0,
                        alive: true,
                        health: this.config.player.health,
                        lastKey: {seq: null, tick: null},
                        outstandingMovementRequests: []
                       };
};

GameEngine.prototype.delPlayer = function (id) {
    delete this.players[id];
    // for (let i=0; i<this.fortresses.length; i++) {
    //     if (this.fortresses[i].playerTarget === id)
    //         this.fortresses[i].playerTarget = null;
    // }
};

GameEngine.prototype.killPlayer = function (p) {
    p.alive = false;
    p.spawnTimer = 0;
    this.ignite(Math.round(p.position.x/this.config.mapCellSize),
                Math.round(p.position.y/this.config.mapCellSize));
};


exports.GameEngine = GameEngine;

exports.MAP_FIRE = MAP_FIRE;
exports.MAP_ASH = MAP_ASH;
exports.MAP_RETARDANT = MAP_RETARDANT;
exports.MAP_WATER = MAP_WATER;
exports.MAP_TREE = MAP_TREE;
exports.MAP_HOUSE = MAP_HOUSE;
exports.MAP_ROCK = MAP_ROCK;
exports.MAP_GRASS = MAP_GRASS;
exports.MAP_HROAD = MAP_HROAD;
exports.MAP_VROAD = MAP_VROAD;
