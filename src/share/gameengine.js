var KEY_LEFT = 1;
var KEY_RIGHT = 2;
var KEY_UP = 3;
var KEY_DOWN = 4;
var KEY_SPACE = 5;
var KEY_1 = 6;
var KEY_2 = 7;
var KEY_3 = 8;
var KEY_4 = 9;

var MESSAGE_1 = 1;
var MESSAGE_2 = 2;
var MESSAGE_3 = 3;
var MESSAGE_4 = 4;

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

function Hexagon (radius) {
    Object.call(this);
    var x1 = Math.floor(-radius);
    var x2 = Math.floor(-radius*0.5);
    var x3 = Math.floor(+radius*0.5);
    var x4 = Math.floor(+radius);
    var y1 = 0;
    var y2 = Math.floor(-radius*Math.sin(Math.PI*2/3));
    var y3 = Math.floor(+radius*Math.sin(Math.PI*2/3));
    this.points = [V(x1,y1),
                   V(x2,y2),
                   V(x3,y2),
                   V(x4,y1),
                   V(x3,y3),
                   V(x2,y3)];
    this.radius = radius;
    return this;
}

Hexagon.prototype = {};

Hexagon.prototype.inside = function (center, v) {
    var i;
    for (i=0; i<this.points.length-1; i++) {
        var nx, ny, dx, dy;
        nx = -(this.points[i+1].y - this.points[i].y);
        ny =   this.points[i+1].x - this.points[i].x;
        dx = v.x - (this.points[i].x+center.x);
        dy = v.y - (this.points[i].y+center.y);
        if (nx * dx + ny * dy < 0) {
            return false;
        }
    }
    return true;
};

Hexagon.prototype.path = function (ctx, x, y, angle) {
    var i;
    ctx.translate(x,y);
    ctx.rotate(deg2rad(angle));
    ctx.beginPath();
    ctx.moveTo(this.points[0].x, this.points[0].y);
    for (i=1; i<this.points.length; i++) {
        ctx.lineTo(this.points[i].x, this.points[i].y);
    }
    ctx.closePath();
};

Hexagon.prototype.fill = function (ctx, x, y, angle, color) {
    ctx.save();
    this.path(ctx, x, y, angle);
    ctx.fillStyle = color || '#000000';
    ctx.fill();
    ctx.restore();
}

Hexagon.prototype.draw = function (ctx, x, y, angle, color) {
    ctx.save();
    this.path(ctx, x, y, angle);
    ctx.strokeStyle = color || '#00FF00';
    ctx.stroke();
    ctx.restore();
};

Hexagon.prototype.drawPartial = function (ctx, x, y, angle, color) {
    var i;
    ctx.save();
    ctx.translate(x,y);
    ctx.rotate(deg2rad(angle));
    ctx.beginPath();
    ctx.strokeStyle = color || '#00FF00';
    ctx.moveTo(this.points[1].x, this.points[1].y);
    for (i=2; i<6; i++) {
        ctx.lineTo(this.points[i].x, this.points[i].y);
    }
    ctx.stroke();
    ctx.restore();
};

function GameEngine(config) {
    this.config = config;
    this.players = {};
    this.fortresses = [];
    this.missiles = [];
    this.shells = [];
    this.asteroids = [];
    this.ticks = 0;
    this.clock = 0;
    this.rawPoints = 0;
    this.points = 0;

    this.messages = [];

    this.hexagons = {};
    this.hexagons[this.config.fortress.bigHex] = new Hexagon(this.config.fortress.bigHex);
    this.hexagons[this.config.fortress.smallHex] = new Hexagon(this.config.fortress.smallHex);
}

GameEngine.prototype = {};

GameEngine.prototype.reward = function (amt) {
    this.rawPoints += amt;
    this.points += amt;
    if (this.points < 0) this.points = 0;
}

GameEngine.prototype.makeAsteroid = function (n) {
    var asteroid = {position: {x:Math.random()*this.config.mapSize*this.config.mapCellSize,
                               y:Math.random()*this.config.mapSize*this.config.mapCellSize},
                    velocity: { x: Math.cos(Math.random()*Math.PI*2) * 0.1,
                                y: Math.sin(Math.random()*Math.PI*2) * 0.1},
                    angularVelocity: Math.random()*0.05-0.1,
                    angle: 0,
                    bubbles: new Array(n)};
    asteroid.bubbles[0] = {x:0, y:0, r: Math.random()*60+10};
    var size = 40;
    for (let i=1; i<n; i++) {
        var r = Math.random() * size+10;
        var a = Math.random()*Math.PI*2;
        var b = {x: asteroid.bubbles[i-1].x + Math.cos(a) * (r+asteroid.bubbles[i-1].r),
                 y: asteroid.bubbles[i-1].y + Math.sin(a) * (r+asteroid.bubbles[i-1].r),
                 r: r};
        asteroid.bubbles[i] = b;
    }

    return asteroid;
}

GameEngine.prototype.placeAsteroids = function (n, tries) {
    this.asteroids = new Array(n);
    for (let i=0; i<n; i++) {
        this.asteroids[i] = this.makeAsteroid(6);
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
                                  config: this.config.fortress,
                                  respawnTimer: 0
                                 });
        if (this.fortresses.length >= n) break;
    }
    console.log('done placing', this.fortresses.length);
};

GameEngine.prototype.createMap = function () {
    this.map = new Array(this.config.mapSize * this.config.mapSize);
};

GameEngine.prototype.stepOneTick = function (ms) {
    this.clock += ms;
    this.ticks += 1;
    this.updatePlayers();
    this.updateFortresses();
    this.updateMissiles();
    this.updateShells();
    this.updateAsteroids();
    // this.updateEntities();
    // this.handleCollisions();
};

GameEngine.prototype.accumPlayerMovementRequests = function (p, req) {
    p.movementRequests.push(req);
}

GameEngine.prototype.processPlayerKeys = function (p, keys) {
    p.missileRequests = 0;
    p.messageRequests = [];
    for (var i=0; i<keys.length; i++) {
        if (keys[i][0] === 1) {
            if (keys[i][1] === KEY_LEFT) p.turnFlag = 'left';
            else if (keys[i][1] === KEY_RIGHT) p.turnFlag = 'right';
            else if (keys[i][1] === KEY_UP) p.thrustFlag = true;
            else if (keys[i][1] === KEY_SPACE) p.missileRequests += 1;
            else if (keys[i][1] === KEY_1) p.messageRequests.push(MESSAGE_1);
            else if (keys[i][1] === KEY_2) p.messageRequests.push(MESSAGE_2);
            else if (keys[i][1] === KEY_3) p.messageRequests.push(MESSAGE_3);
            else if (keys[i][1] === KEY_4) p.messageRequests.push(MESSAGE_4);

            // else if (keys[i][1] === KEY_1) this.say(p, MESSAGE_1);
            // else if (keys[i][1] === KEY_2) this.say(p, MESSAGE_2);
            // else if (keys[i][1] === KEY_3) this.say(p, MESSAGE_3);
            // else if (keys[i][1] === KEY_4) this.say(p, MESSAGE_4);
        } else {
            if (keys[i][1] === KEY_LEFT || keys[i][1] === KEY_RIGHT) p.turnFlag = null;
            else if (keys[i][1] === KEY_UP) p.thrustFlag = false;
            else if (keys[i][1] === KEY_SPACE) p.missileState = false;
        }
    }
};

GameEngine.prototype.say = function (p, msg) {
    this.messages.push([p.id, msg]);
}

GameEngine.prototype.applyPlayerMovements = function (p) {
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

};

GameEngine.prototype.updatePlayer = function (p) {
    if (p.alive) {
        this.applyPlayerMovements(p);

        if (p.missileRequests > 0) {
            this.addMissile(p);
            p.missileRequests = 0;
        }
        for (let i=0; i<p.messageRequests.length; i++) {
            this.say(p, p.messageRequests[i]);
        }
        p.messageRequests.length = 0;

        if (p.position.x < 0 ||
            p.position.x > this.config.mapSize * this.config.mapCellSize ||
            p.position.y < 0 ||
            p.position.y > this.config.mapSize * this.config.mapCellSize) {
            this.killPlayer(p);
        }
        for (let i=0; i<this.fortresses.length; i++) {
            if (this.fortresses[i].alive &&
                distance(this.fortresses[i].position, p.position) < this.config.player.collisionRadius+this.config.fortress.collisionRadius) {
                this.killPlayer(p);
                break;
            }
        }
        // console.log(p);
    } else {
        p.spawnTimer += 1;
        if (p.spawnTimer >= this.config.player.deathTimer) {
            p.alive = true;
            p.position.x = this.config.player.startPosition.x;
            p.position.y = this.config.player.startPosition.y;
            p.velocity.x = this.config.player.startVelocity.x;
            p.velocity.y = this.config.player.startVelocity.y;
            p.angle = this.config.player.startAngle;
        }
    }
};

GameEngine.prototype.addMissile = function (owner) {
    // if (!owner.missileState) {
        var m = {owner: owner.id,
                 position: {x: owner.position.x,
                            y: owner.position.y},
                 velocity: {x: Math.cos(deg2rad(owner.angle)) * this.config.missile.speed + owner.velocity.x,
                            y: Math.sin(deg2rad(owner.angle)) * this.config.missile.speed + owner.velocity.y},
                 angle: owner.angle,
                 alive: true,
                 spawnTick: this.ticks};
        this.missiles.push(m)
        // owner.missileState = true;
    // }
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
                        this.reward(this.config.rewards.fortressDestroy);
                        f.alive = false;
                        f.respawnTimer = 0;
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
    } else {
        f.respawnTimer += 1;
        if (f.respawnTimer >= this.config.fortress.respawnTime)
            f.alive = true;
    }
};

GameEngine.prototype.updatePlayers = function () {
    for (let id in this.players) {
        var p = this.players[id];
        for (let i=0; i<p.movementRequests.length; i++) {
            p.lastMovementRequest = p.movementRequests[i][0];
            p.thrustFlag = p.movementRequests[i][1];
            p.turnFlag = p.movementRequests[i][2];
            p.missileRequests = p.movementRequests[i][3];
            p.messageRequests = p.movementRequests[i][4];
            this.updatePlayer(p);
        }
        p.movementRequests.length = 0;
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
    var n = Object.keys(this.players).length;
    this.players[id] = {id: id,
                        angle: this.config.player.startAngle,
                        position: {x: this.config.player.startPosition.x,
                                   y: this.config.player.startPosition.y},
                        velocity: {x: this.config.player.startVelocity.x,
                                   y: this.config.player.startVelocity.y},
                        config: this.config.player,
                        turnFlag: 0,
                        thrustFlag: 0,
                        // missileState: 0,
                        messageRequests: [],
                        missileRequests: 0,
                        Requests: [],
                        spawnTimer: 0,
                        alive: true,
                        color: this.config.player.colors[n%this.config.player.colors.length],
                        lastMovementRequest: -1,
                        movementRequests: []
                       };
};

GameEngine.prototype.delPlayer = function (id) {
    delete this.players[id];
    for (let i=0; i<this.fortresses.length; i++) {
        if (this.fortresses[i].playerTarget === id)
            this.fortresses[i].playerTarget = null;
    }
};

GameEngine.prototype.killPlayer = function (p) {
    p.alive = false;
    p.spawnTimer = 0;
    this.reward(this.config.rewards.shipDeath);
};

GameEngine.prototype.gameStateColumnTitles = function () {
    // FIXME: this could get out of sync with dumpState()
    var titles = new Array(5);

    titles[0] = 'game_ticks';
    titles[1] = 'game_clock';
    titles[2] = ['player_alive',
                 'player_x',
                 'player_y',
                 'player_vx',
                 'player_vy',
                 'player_turnFlag',
                 'player_thrustFrag'];
    titles[3] = ['fortress_alive',
                 'fortress_angle',
                 'fortress_playerTarget',
                 'fortress_missileTarget'];
    titles[4] = ['asteroid_angle',
                 'asteroid_x',
                 'asteroid_y',
                 'asteroid_vx',
                 'asteroid_vy'];
    return titles;
};

GameEngine.prototype.dumpState = function () {
    var numPlayers = 2;
    var state = new Array(5);

    state[0] = this.ticks;
    state[1] = this.clock;

    var j = 0;
    state[2] = new Array(numPlayers);
    for (let k in this.players) {
        state[2][j] = [this.players[k].alive,
                       this.players[k].angle,
                       this.players[k].position.x,
                       this.players[k].position.y,
                       this.players[k].velocity.x,
                       this.players[k].velocity.y,
                       this.players[k].turnFlag,
                       this.players[k].thrustFlag];
    }

    state[3] = new Array(this.fortresses.length);
    for (let i=0; i<this.fortresses.length; i++) {
        state[3][i] = [this.fortresses[i].alive,
                       this.fortresses[i].angle,
                       this.fortresses[i].playerTarget ? this.fortresses[i].playerTarget.id:-1,
                       this.fortresses[i].missileTarget ? this.fortresses[i].missileTarget.position:-1];
    }

    state[4] = new Array(this.asteroids.length);
    for (let i=0; i<this.asteroids.length; i++) {
        state[4][i] = [this.asteroids[i].angle,
                       this.asteroids[i].position.x,
                       this.asteroids[i].position.y,
                       this.asteroids[i].velocity.x,
                       this.asteroids[i].velocity.y];
    }

    return state;
};

exports.GameEngine = GameEngine;
