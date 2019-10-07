var KEY_LEFT = 1;
var KEY_RIGHT = 2;
var KEY_UP = 3;
var KEY_DOWN = 4;
var KEY_SPACE = 5;
var KEY_WP_WATER = 6;
var KEY_WP_HOUSE = 7;
var KEY_WP_FIRE = 8;
var KEY_WP_UNKNOWN = 9;

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
var MAP_RUNWAY = 11;

var UPDATE_UPDATE = 0;
var UPDATE_ADD = 1;
var UPDATE_DEL = 2;

var PLAYER_SERVER_UPDATE_KEYS = ['alive', ['position', 'x'], ['position', 'y'], 'speed', 'turnFlag'];

// http://paulbourke.net/geometry/lineline2d/
var LINES_PARALLEL = 0;
var INTERSECTION_INSIDE = 1;
var LINES_COINCIDE = 2;
var INTERSECTION_OUTSIDE_SEG1 = 3;
var INTERSECTION_OUTSIDE_SEG2 = 4;
var INTERSECTION_OUTSIDE_BOTH = 5;
function lines_intersection_point(p1, p2, p3, p4) {
    var out;
    var eps = 0.000000000001;

    var denom  = (p4.y-p3.y) * (p2.x-p1.x) - (p4.x-p3.x) * (p2.y-p1.y);
    var numera = (p4.x-p3.x) * (p1.y-p3.y) - (p4.y-p3.y) * (p1.x-p3.x);
    var numerb = (p2.x-p1.x) * (p1.y-p3.y) - (p2.y-p1.y) * (p1.x-p3.x);

    if ( (-eps < numera && numera < eps) &&
         (-eps < numerb && numerb < eps) &&
         (-eps < denom  && denom  < eps) ) {
        out = {x:(p1.x + p2.x) * 0.5, y:(p1.y + p2.y) * 0.5};
        return [LINES_COINCIDE, out];
    }

    if (-eps < denom  && denom  < eps) {
	return [LINES_PARALLEL, null];
    }

    var mua = numera / denom;
    var mub = numerb / denom;

    out = {x:p1.x + mua * (p2.x - p1.x),
           y: p1.y + mua * (p2.y - p1.y)};
    var out1 = mua < 0 || mua > 1;
    var out2 = mub < 0 || mub > 1;

    if ( out1 & out2) {
	return [INTERSECTION_OUTSIDE_BOTH, out];
    } else if ( out1) {
	return [INTERSECTION_OUTSIDE_SEG1, out];
    } else if ( out2) {
	return [INTERSECTION_OUTSIDE_SEG2, out];
    } else {
	return [INTERSECTION_INSIDE, out];
    }
}


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
    this.map = {width: 10,
                height: 10,
                viewPort: {x:0,y:0,w:10,h:10},
                updates: [],
                retardant: [],
                retardantUpdates: [],
                fire: [],
                fireUpdates: [],
                fireTimer: 0,
                timeout: this.config.map.resizeDuration,
                viewPortUpdated: false,
                wayPoints: [],
                wayPointUpdates: []};
    this.map.data = new Array(this.map.width * this.map.height);
    this.wind = {x:0, y:0};
    this.ticks = 0;
}

GameEngine.prototype = {};

GameEngine.prototype.mapAt = function (x,y) {
    if (x>=0 && x < this.map.width &&
        y>=0 && y < this.map.height)
        return this.map.data[y*this.map.width+x];
};

GameEngine.prototype.mapSet = function (x,y,r) {
    if (x>=0 && x < this.map.width &&
        y>=0 && y < this.map.height) {
        var idx = y*this.map.width+x;
        if (this.map.data[idx] !== r) {
            this.map.data[idx] = r;
            this.map.updates.push(idx);
        }
    }
};

GameEngine.prototype.createMap = function () {
    for (let y=0; i<this.map.height; i++) {
        for (let x=0; i<this.map.width; i++) {
            this.map.data[y*this.map.width + x] = 0;
        }
    }
};

GameEngine.prototype.startSomeFires = function () {
    var n = this.config.map.startFires;
    for (let i=0; i<n; i++) {
        var x = Math.round(Math.random()*this.map.viewPort.w) + this.map.viewPort.x;
        var y = Math.round(Math.random()*this.map.viewPort.h) + this.map.viewPort.y;
        this.ignite(x,y);
        this.ignite(x+1,y);
        this.ignite(x,y+1);
        this.ignite(x-1,y);
        this.ignite(x,y-1);
    }
    this.map.fireUpdates.length = 0;
};

GameEngine.prototype.stepOneTick = function () {
    this.ticks += 1;
    this.updatePlayers();
    this.updateRetardant();
    this.updateFires();

    this.updateMapTimeout();
};

GameEngine.prototype.isFinished = function () {
    return this.map.fire.length <= 0;
};

GameEngine.prototype.getScore = function () {
    var count = 0;
    for (let i=0; i<this.map.data.length; i++) {
        if (this.map.data[i] !== MAP_ASH) count += 1;
    }
    return count;
};

GameEngine.prototype.retardantCanBePlaced = function (x, y, batch) {
    for (let i=0; i<batch.length; i++) {
        if (batch[i].x === x &&
            batch[i].y === y) {
            return false;
        }
    }
    return true;
}

GameEngine.prototype.retardantLevelAt = function (x, y) {
    for (let i=0; i<this.map.retardant.length; i++) {
        if (this.map.retardant[i].x === x &&
            this.map.retardant[i].y === y) {
            return this.map.retardant[i].level;
        }
    }
    return 0;
};

GameEngine.prototype.retardantAt = function (x, y) {
    for (let i=0; i<this.map.retardant.length; i++) {
        if (this.map.retardant[i].x === x &&
            this.map.retardant[i].y === y) {
            return this.map.retardant[i];
        }
    }
    return null;
};

GameEngine.prototype.addRetardant = function (x, y, batch) {
    for (let i=0; i<batch.length; i++) {
        if (batch[i].x === x &&
            batch[i].y === y) {
            return;
        }
    }

    var timeout = this.ticks + this.config.retardant.duration + Math.floor(Math.random()*this.config.retardant.durJitter - this.config.retardant.durJitter/2);

    for (let i=0; i<this.map.retardant.length; i++) {
        if (this.map.retardant[i].x === x &&
            this.map.retardant[i].y === y) {
            var r = this.map.retardant[i];
            r.amt += 1;
            r.timeout = timeout;
            this.map.retardantUpdates.push([UPDATE_UPDATE, r]);
            batch.push(r);
            return;
        }
    }
    var r = {x:x, y:y, amt:1, timeout:timeout};
    this.map.retardant.push (r);
    this.map.retardantUpdates.push([UPDATE_ADD, r]);
    batch.push(r);
};

GameEngine.prototype.ignite = function (x, y) {
    var m = this.mapAt(x,y);
    if (m === undefined ||
        m === MAP_ROCK ||
        m === MAP_HROAD ||
        m === MAP_VROAD ||
        m === MAP_WATER ||
        m === MAP_ASH ||
        m === MAP_RETARDANT)
        return;
    for (let i=0; i<this.map.fire.length;i++)
        if (this.map.fire[i].x === x &&
            this.map.fire[i].y === y)
            return;
    var f = {x:x, y:y, level: 1};
    this.map.fire.push(f);
    this.map.fireUpdates.push([UPDATE_ADD, f]);
}

GameEngine.prototype.extinguish = function (idx, reduction) {
    var f = this.map.fire[idx];
    f.level -= reduction;
    // if (f.level < 0) {
    this.map.fire.splice(idx, 1);
    this.map.fireUpdates.push([UPDATE_DEL, f]);
    // }
};

GameEngine.prototype.updateRetardant = function () {
    for (let i=this.map.retardant.length-1; i>=0; i--) {
        if (this.ticks >= this.map.retardant[i].timeout) {
            var r = this.map.retardant[i];
            this.map.retardant.splice(i,1);
            this.map.retardantUpdates.push([UPDATE_DEL,r]);
            var c = 0;
            for (let j=this.map.fire.length-1; j>=0; j--) {
                if (Math.abs(r.x-this.map.fire[j].x) <= 1 &&
                    Math.abs(r.y-this.map.fire[j].y) <= 1) {
                // if (r.x === this.map.fire[j].x &&
                //     r.y === this.map.fire[j].y) {
                    this.extinguish(j, r.amt);
                    c += 1;
                    // console.log('extinguished', r.x,r.y);
                }
            }
            // console.log('howmany', c, r.x, r.y);
        }
    }
};

GameEngine.prototype.fireSanity = function (msg) {
    var dups = 0;
    for (let i=0; i<this.map.fire.length; i++)
        for (let j=i+1; j<this.map.fire.length; j++) {
            if (this.map.fire[i].x === this.map.fire[j].x &&
                this.map.fire[i].y === this.map.fire[j].y)
                dups += 1;
        }
    if (dups > 0)
        console.log(msg, 'duplicate fires', dups);
};

GameEngine.prototype.updateFires = function () {
    // this.fireSanity('');
    this.map.fireTimer -= 1;
    if (this.map.fire.length > 0 && this.map.fireTimer <= 0) {
        var n = 1;
        for (let i=0; i<n; i++) {
            var idx = Math.floor(Math.random() * this.map.fire.length);
            var f = this.map.fire[idx];
            if (!f) console.log(this.map.fire.length, f, idx);
            this.map.fire.splice(idx,1);
            this.map.fireUpdates.push([UPDATE_DEL, f]);
            this.mapSet(f.x, f.y, MAP_ASH);
            if (Math.random() <= this.config.fire.spreadingOdds) {
                this.ignite(f.x+1,f.y);
                this.ignite(f.x,f.y+1);
                this.ignite(f.x-1,f.y);
                this.ignite(f.x,f.y-1);
            }
        }
        this.map.fireTimer = this.config.fire.ticks;
    }
    //     } if (m === MAP_RETARDANT) {
    //         var r1 = this.extinguish(f.x+1,f.y);
    //         var r2 = this.extinguish(f.x,f.y+1);
    //         var r3 = this.extinguish(f.x-1,f.y);
    //         var r4 = this.extinguish(f.x,f.y-1);
    //         if (r1||r2||r3||r4) this.mapSet(f.x, f.y, MAP_GRASS);
    //     }
    // }
};

GameEngine.prototype.updateMapTimeout = function () {
    if (this.ticks >= this.map.timeout) {
        this.map.timeout = this.ticks + this.config.map.resizeDuration;
        var amt = this.config.map.resizeAmount;
        this.map.viewPort.x -= amt;
        this.map.viewPort.y -= amt;
        this.map.viewPort.w += amt*2;
        this.map.viewPort.h += amt*2;
        if (this.map.viewPort.x < 0) this.map.viewPort.x = 0;
        if (this.map.viewPort.y < 0) this.map.viewPort.y = 0;
        if (this.map.viewPort.w > this.map.width) this.map.viewPort.w = this.map.width;
        if (this.map.viewPort.h > this.map.height) this.map.viewPort.h = this.map.height;
        this.map.viewPortUpdated = true;
    }
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
            else if (keys[i][1] === KEY_WP_WATER) p.wayPointFlags.push(MAP_WATER);
            else if (keys[i][1] === KEY_WP_HOUSE) p.wayPointFlags.push(MAP_HOUSE);
            else if (keys[i][1] === KEY_WP_FIRE) p.wayPointFlags.push(MAP_FIRE);
        } else {
            if (keys[i][1] === KEY_LEFT) {
                if (p.turnFlag == 'l') p.turnFlag = 0;
            } else if (keys[i][1] === KEY_RIGHT) {
                if (p.turnFlag == 'r') p.turnFlag = 0;
            } else if (keys[i][1] === KEY_UP) p.thrustFlag = 0;
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
    p.position.x = this.map.width * this.config.map.cellSize / 2;
    p.position.y = this.map.height * this.config.map.cellSize / 2;
    p.speed = 0;
    p.angle = this.config.player.startAngle;
    p.health = this.config.player.health;
    p.water = this.config.player.maxWater;
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
        var mx = Math.round(p.position.x/this.config.map.cellSize),
            my = Math.round(p.position.y/this.config.map.cellSize),
            m = this.mapAt(mx, my);
        p.speed -= 0.1;
        var min;
        if (m === MAP_RUNWAY) min = 0;
        else min = 1;
        if (p.speed < min) p.speed = min;
    }

    p.position.x += p.speed * Math.cos(deg2rad(p.angle));
    p.position.y += p.speed * Math.sin(deg2rad(p.angle));
};

GameEngine.prototype.playerApplyDump = function (p) {
    if (p.waterFillTimer > 0) p.waterFillTimer -= 1;
    if (p.dumpFlag) {
        var mx = Math.round(p.position.x/this.config.map.cellSize),
            my = Math.round(p.position.y/this.config.map.cellSize),
            m = this.mapAt(mx, my);
        if (m === MAP_WATER) {
            if (p.waterFillTimer <= 0 && p.speed < 2) {
                p.water += 1;
                p.waterFillTimer = p.config.waterFillRate;
            }
            if (p.water > p.config.maxWater)
                p.water = p.config.maxWater;
        } else if (p.water > 0 &&
                   this.retardantCanBePlaced(mx,my,p.retardantBatch)) {
            this.addRetardant(mx, my, p.retardantBatch);
            p.water -= 1;
            if (p.water < 0) p.water = 0;
        }
    } else {
        p.retardantBatch.length = 0;
    }
};

GameEngine.prototype.playerApplyWaypoint = function (p, wp) {
    var mx = Math.round(p.position.x/this.config.map.cellSize)*this.config.map.cellSize,
        my = Math.round(p.position.y/this.config.map.cellSize)*this.config.map.cellSize;
    var o = {tick: this.ticks,
             type: wp,
             x: mx,
             y: my};
    this.map.wayPoints.push(o);
    this.map.wayPointUpdates.push([UPDATE_ADD, o]);
};

GameEngine.prototype.processOutstandingPlayerMovements = function (p) {
    for (let i=0; i<p.outstandingMovementRequests.length; i++) {
        p.lastKey.seq = p.outstandingMovementRequests[i][0];
        p.turnFlag = p.outstandingMovementRequests[i][1];
        p.thrustFlag = p.outstandingMovementRequests[i][2];
        p.dumpFlag = p.outstandingMovementRequests[i][3];
        for (let j=0; j<p.outstandingMovementRequests[i][4].length; j++) {
            this.playerApplyWaypoint(p, p.outstandingMovementRequests[i][4][j]);
        }
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
        //     p.position.x > this.map.width * this.config.map.cellSize ||
        //     p.position.y < 0 ||
        //     p.position.y > this.map.height * this.config.map.cellSize) {
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
                        position: {x: this.map.width * this.config.map.cellSize / 2,
                                   y: this.map.height * this.config.map.cellSize / 2},

                        speed: 0,
                        config: this.config.player,
                        turnFlag: 0,
                        thrustFlag: 0,
                        water: this.config.player.maxWater,
                        waterFillTimer: 0,
                        spawnTimer: 0,
                        alive: true,
                        health: this.config.player.health,
                        lastKey: {seq: null, tick: null},
                        outstandingMovementRequests: [],
                        retardantBatch: []
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
    this.ignite(Math.round(p.position.x/this.config.map.cellSize),
                Math.round(p.position.y/this.config.map.cellSize));
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
