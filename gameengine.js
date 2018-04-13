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
        this.map[y*this.config.mapSize+x] = r;
        this.mapUpdates.push(y*this.config.mapSize+x);
    }
};

GameEngine.prototype.createMap = function () {
    for (let y=0; i<this.config.mapSize; i++) {
        for (let x=0; i<this.config.mapSize; i++) {
            this.map[y*this.config.mapSize + x] = 0;
        }
    }
};

GameEngine.prototype.placeFortresses = function (n, tries) {
    this.fortresses = [];
    console.log('start placing', this.fortresses.length);
    for (let i=0; i<tries; i++) {
        var pos = {x: Math.random()*(this.config.mapSize*this.config.mapCellSize-this.config.fortress.bigHex*2)+this.config.fortress.bigHex,
                   y: Math.random()*(this.config.mapSize*this.config.mapCellSize-this.config.fortress.bigHex*2)+this.config.fortress.bigHex}
        var ok = true;
        for (let j=0; j<this.fortresses.length; j++) {
            if (distance(this.fortresses[j].position, pos) < this.config.fortress.bigHex*2) {
                ok = false;
                break;
            }
        }
        if (ok)
            this.fortresses.push({alive: true,
                                  position: pos,
                                  angle: 0,
                                  playerTarget: null,
                                  missileTarget: null,
                                  radius: this.config.fortress.bigHex,
                                  config: this.config.fortress
                                 });
        if (this.fortresses.length >= n) break;
    }
    console.log('done placing', this.fortresses.length);
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

    // this.updateFortresses();
    // this.updateMissiles();
    // this.updateShells();
    // this.updateAsteroids();
    // this.updateEntities();
    // this.handleCollisions();
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
};

GameEngine.prototype.updateSparks = function () {

};

GameEngine.prototype.processPlayerKeys = function (p, keys) {
    for (var i=0; i<keys.length; i++) {
        // console.log(keys[i][0] ? 'pressed':'released', keys[i][1]);
        if (keys[i][0] === 1) {
            if (keys[i][1] === KEY_LEFT) p.turnFlag = 'left';
            else if (keys[i][1] === KEY_RIGHT) p.turnFlag = 'right';
            else if (keys[i][1] === KEY_UP) p.thrustFlag = 1;
            else if (keys[i][1] === KEY_DOWN) p.thrustFlag = -1;
            else if (keys[i][1] === KEY_SPACE) p.dumpFlag = true;
        } else {
            if (keys[i][1] === KEY_LEFT || keys[i][1] === KEY_RIGHT) p.turnFlag = null;
            else if (keys[i][1] === KEY_UP) p.thrustFlag = 0;
            else if (keys[i][1] === KEY_DOWN) p.thrustFlag = 0;
            else if (keys[i][1] === KEY_SPACE) p.dumpFlag = false;
        }
    }
};

GameEngine.prototype.resetPlayer = function (p) {
    p.alive = true;
    p.position.x = this.config.player.startPosition.x;
    p.position.y = this.config.player.startPosition.y;
    p.speed = 0;
    p.angle = this.config.player.startAngle;
};

GameEngine.prototype.updatePlayer = function (p) {
    if (p.alive) {
        if (p.turnFlag === 'left') {
            p.angle -= p.config.turnRate;
            p.angle = stdAngle(p.angle);
        } else if (p.turnFlag === 'right') {
            p.angle += p.config.turnRate;
            p.angle = stdAngle(p.angle);
        }
        if (p.thrustFlag === 1) {
            p.speed += 0.1;
            if (p.speed > this.config.player.maxSpeed) p.speed = this.config.player.maxSpeed;
        } else if (p.thrustFlag === -1) {
            p.speed -= 0.1;
            if (p.speed < 0) p.speed = 0;
        }

        p.position.x += p.speed * Math.cos(deg2rad(p.angle));
        p.position.y += p.speed * Math.sin(deg2rad(p.angle));

        if (p.dumpFlag) {
            var mx = Math.round(p.position.x/this.config.mapCellSize),
                my = Math.round(p.position.y/this.config.mapCellSize),
                m = this.mapAt(mx, my);
            if (m !== MAP_WATER) {
                this.mapSet(mx, my, MAP_RETARDANT);
            }
        }

        if (p.position.x < 0 ||
            p.position.x > this.config.mapSize * this.config.mapCellSize ||
            p.position.y < 0 ||
            p.position.y > this.config.mapSize * this.config.mapCellSize) {
            this.killPlayer(p);
        }
    } else {
        p.spawnTimer += 1;
        if (p.spawnTimer >= this.config.player.deathTimer) {
            this.resetPlayer(p);
        }
    }
};

GameEngine.prototype.addMissile = function (owner) {
    if (!owner.missileState) {
        var m = {owner: owner.id,
                 position: {x: owner.position.x,
                            y: owner.position.y},
                 velocity: {x: Math.cos(deg2rad(owner.angle)) * this.config.missile.speed + owner.velocity.x,
                            y: Math.sin(deg2rad(owner.angle)) * this.config.missile.speed + owner.velocity.y},
                 angle: owner.angle,
                 alive: true,
                 spawnTick: this.ticks};
        this.missiles.push(m)
        owner.missileState = true;
    }
};

GameEngine.prototype.addShell = function (owner) {
    var m = {position: {x: owner.position.x,
                        y: owner.position.y},
             velocity: {x: Math.cos(deg2rad(owner.angle)) * this.config.shell.speed,
                        y: Math.sin(deg2rad(owner.angle)) * this.config.shell.speed},
             angle: owner.angle,
             alive: true,
             spawnTick: this.ticks};
    this.shells.push(m);
};

GameEngine.prototype.killMissile = function (m) {
    m.alive = false;
    for (let i=0; i<this.fortresses.length; i++) {
        if (this.fortresses[i].missileTarget === m)
            this.fortresses[i].missileTarget = null;
    }
};

GameEngine.prototype.updateMissiles = function () {
    var acc = [];
    for (let i=0; i<this.missiles.length; i++) {
        var m = this.missiles[i];
        if (m.alive) {
            m.position.x += m.velocity.x;
            m.position.y += m.velocity.y;
            if (m.position.x < 0 ||
                m.position.x > this.config.mapSize * this.config.mapCellSize ||
                m.position.y < 0 ||
                m.position.y > this.config.mapSize * this.config.mapCellSize) {
                this.killMissile(m);
                continue;
            }
            if (this.ticks - m.spawnTick > this.config.missile.lifespan) {
                this.killMissile(m);
                continue;
            }
            for (let i=0; i<this.fortresses.length; i++) {
                var f = this.fortresses[i];
                if (f.alive && distance(f.position, m.position) < this.config.fortress.smallHex) {
                    // if (f.alive && this.hexagons[this.config.fortress.smallHex].inside(f.position, m.position)) {
                    this.killMissile(m);
                    var to = angleTo(f.position, m.position);
                    var a = angle_diff(f.angle, to);
                    if (a > 120 || a < -120) {
                        f.alive = false;
                        // console.log(Math.round(f.angle), Math.round(to), a);
                    }
                    break;
                }
            }
        }
        if (m.alive) acc.push(m);
    }
    this.missiles = acc;
};

GameEngine.prototype.updateShells = function () {
    var acc = [];
    for (let i=0; i<this.shells.length; i++) {
        var s = this.shells[i];
        s.position.x += s.velocity.x;
        s.position.y += s.velocity.y;
        if (s.position.x < 0 ||
            s.position.x > this.config.mapSize * this.config.mapCellSize ||
            s.position.y < 0 ||
            s.position.y > this.config.mapSize * this.config.mapCellSize) {
            s.alive = false;
            continue;
        }
        if (this.ticks - s.spawnTick > this.config.shell.lifespan) {
            s.alive = false;
            continue;
        }
        for (let k in this.players) {
            if (distance(s.position, this.players[k].position) < this.config.player.collisionRadius+this.config.shell.collisionRadius) {
                this.killPlayer(this.players[k]);
                s.alive = false;
                break;
            }
        }
        if (s.alive) acc.push(s);
    }
    this.shells = acc;
};

GameEngine.prototype.updateFortresses = function () {
    for (let i=0; i<this.fortresses.length; i++) {
        this.updateFortress(this.fortresses[i]);
    }
};

GameEngine.prototype.fortressAimAt = function (f, pos) {
    var a = angleTo(f.position, pos);
    var d = angle_diff(a,f.angle);
    if (d < 0) {
        if (-d > this.config.fortress.lockSpeed)
            f.angle -= this.config.fortress.lockSpeed;
        else
            f.angle = a;
    } else {
        if (d > this.config.fortress.lockSpeed)
            f.angle += this.config.fortress.lockSpeed;
        else
            f.angle = a;
    }
};

GameEngine.prototype.updateFortress = function (f) {
    if (f.alive) {
        if (f.playerTarget) {
            if (!this.players[f.playerTarget].alive ||
                // !this.hexagons[f.radius].inside(f.position, this.players[f.playerTarget].position)) {
                distance(f.position, this.players[f.playerTarget].position) > f.radius) {
                f.playerTarget = null;
            }
        }
        if (f.missileTarget) {
            if (!f.missileTarget.alive ||
                // !this.hexagons[f.radius].inside(f.position, this.missileTarget.position)) {
                distance(f.position, f.missileTarget.position) > f.radius) {
                f.missileTarget = null;
            }
        }
        if (!f.playerTarget) {
            for (k in this.players) {
                if (this.players[k].alive &&
                    // this.hexagons[f.radius].inside(f.position, this.players[k].position)) {
                    distance(f.position, this.players[k].position) <= f.radius) {
                    f.playerTarget = k;
                    f.targetTimer = 0;
                    break;
                }
            }
        }
        if (!f.playerTarget) {
            for (let i=0; i<this.missiles.length; i++) {
                // if (this.hexagons[f.radius].inside(f.position, this.missiles[i].position)) {
                if (distance(f.position, this.missiles[i].position) <= f.radius) {
                    f.missileTarget = this.missiles[i];
                    break;
                }
            }
        }

        var target = null;
        if (f.playerTarget) target = this.players[f.playerTarget];
        else if (f.missileTarget) target = f.missileTarget;

        if (f.playerTarget) {
            // f.angle = angleTo(f.position, this.players[f.playerTarget].position);
            this.fortressAimAt(f, this.players[f.playerTarget].position);
            f.targetTimer += 1;
            if (f.targetTimer > this.config.fortress.lockTime) {
                this.addShell(f);
                f.targetTimer = 0;
            }
        } else if (f.missileTarget) {
            this.fortressAimAt(f, f.missileTarget.position);
        } else {
            f.angle = stdAngle(f.angle+0.25);
        }
    }
};

GameEngine.prototype.updatePlayers = function () {
    for (let id in this.players) {
        this.updatePlayer(this.players[id]);
    }
};

GameEngine.prototype.updateAsteroids = function () {
    for (let i=0; i<this.asteroids.length; i++) {
        this.asteroids[i].position.x += this.asteroids[i].velocity.x;
        this.asteroids[i].position.y += this.asteroids[i].velocity.y;
        this.asteroids[i].angle = stdAngle(this.asteroids[i].angle+this.asteroids[i].angularVelocity);

        if (this.asteroids[i].position.x < 0) this.asteroids[i].velocity.x *= -1;
        if (this.asteroids[i].position.y < 0) this.asteroids[i].velocity.y *= -1;
        if (this.asteroids[i].position.x > this.config.mapSize*this.config.mapCellSize) this.asteroids[i].velocity.x *= -1;
        if (this.asteroids[i].position.y > this.config.mapSize*this.config.mapCellSize) this.asteroids[i].velocity.y *= -1;



        for (let b=0; b<this.asteroids[i].bubbles.length; b++) {
            var pos = rotate_translate(this.asteroids[i].position.x,
                                       this.asteroids[i].position.y,
                                       this.asteroids[i].bubbles[b].x,
                                       this.asteroids[i].bubbles[b].y,
                                       this.asteroids[i].angle);
            for (let j=0; j<this.missiles.length; j++) {
                if (this.missiles[j].alive && distance(this.missiles[j].position, pos) < this.asteroids[i].bubbles[b].r) {
                    this.missiles[j].alive = false;
                }
            }
            for (let k in this.players) {
                if (this.players[k].alive) {
                    if (distance(this.players[k].position, pos) < this.asteroids[i].bubbles[b].r) {
                        this.killPlayer(this.players[k]);
                    }
                }
            }
        }
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
                        water: 0,
                        spawnTimer: 0,
                        alive: true
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
