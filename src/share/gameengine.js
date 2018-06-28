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

function Wall(x, y, w, h) {
    this.x1 = x;
    this.y1 = y;
    this.x2 = x + w;
    this.y2 = y + h;
}

Wall.prototype = {};

Wall.prototype.translate = function (ofsx, ofsy) {
    this.x1 += ofsx;
    this.x2 += ofsx;
    this.y1 += ofsy;
    this.y2 += ofsy;
    return this;
};

Wall.prototype.rotate = function (r) {
    var s = Math.sin(r * Math.PI / 180);
    var c = Math.cos(r * Math.PI / 180);

    var x1 = this.x1 * c - this.y1 * s;
    var y1 = this.x1 * s + this.y1 * c;
    var x2 = this.x2 * c - this.y2 * s;
    var y2 = this.x2 * s + this.y2 * c;
    this.x1 = x1;
    this.y1 = y1;
    this.x2 = x2;
    this.y2 = y2;
    return this;
}

Wall.prototype.copy = function () {
    // FIXME: ugly.
    var w = new Wall(0, 0, 0, 0);
    w.x1 = this.x1;
    w.y1 = this.y1;
    w.x2 = this.x2;
    w.y2 = this.y2;
    return w;
}

function GameEngine(config) {
    this.config = config;
    this.players = [];
    this.fortresses = [];
    this.missiles = [];
    this.shells = [];
    this.asteroids = [];
    this.walls = [];
    this.startLocations = [];
    this.ticks = 0;
    this.clock = 0;
    this.rawPoints = 0;
    this.points = 0;

    this.messages = [];
    this.events = [];

    this.hexagons = {};
    this.hexagons[this.config.fortress.bigHex] = new Hexagon(this.config.fortress.bigHex);
    this.hexagons[this.config.fortress.smallHex] = new Hexagon(this.config.fortress.smallHex);
}

GameEngine.prototype = {};

GameEngine.prototype.getPlayer = function (id) {
    for (let i=0; i<this.players.length;i++) {
        if (this.players[i].id === id) return this.players[i];
    }
    return null;
}

GameEngine.prototype.getPlayerIndex = function (p) {
    for (let i=0; i<this.players.length; i++) {
        if (this.players[i] === p) return i;
    }
    return -1;
};

GameEngine.prototype.getMissileIndex = function (m) {
    for (let i=0; i<this.missiles.length; i++) {
        if (this.missiles[i] === m) return i;
    }
    return -1;
};

GameEngine.prototype.getFortressIndex = function (f) {
    for (let i=0; i<this.fortresses.length; i++) {
        if (this.fortresses[i] === f) return i;
    }
    return -1;
};


GameEngine.prototype.addEvent = function (thing) {
    this.events.push(thing);
};

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
};

GameEngine.prototype.setStartLocations = function (n) {
    this.startLocations = new Array(n);
    var sz = 25, pad = 150;
    var bounds = this.config.mapSize * this.config.mapCellSize;

    this.startLocations[0] = { x: bounds/4 + pad, y: bounds/4 + pad,
                               w: sz*2, h: sz*2,
                               color: this.config.player.colors[0 % this.config.player.colors.length]};

    this.startLocations[1] = { x: bounds/4 + pad, y: bounds*3/4 - pad,
                               w: sz*2, h: sz*2,
                               color: this.config.player.colors[1 % this.config.player.colors.length]};
};

GameEngine.prototype.placeAsteroids = function (n, tries) {
    this.asteroids = new Array(n);
    for (let i=0; i<n; i++) {
        this.asteroids[i] = this.makeAsteroid(6);
    }
};

GameEngine.prototype.placeWalls = function () {
    var bounds = this.config.mapSize * this.config.mapCellSize;
    var thickness = 10;
    // boundary
    this.walls.push(new Wall(0-thickness/2, 0-thickness/2, thickness, bounds));
    this.walls.push(new Wall(bounds-thickness/2, 0-thickness/2, thickness, bounds));
    this.walls.push(new Wall(thickness/2, 0-thickness/2, bounds, thickness));
    this.walls.push(new Wall(thickness/2, bounds-thickness/2, bounds, thickness));
    // inner wall
    var base = [new Wall(-thickness/2, -thickness/2, thickness, bounds/6),
                new Wall(-thickness/2, -thickness/2, bounds/6, thickness)];
    this.walls.push(base[0].copy().translate(bounds/4, bounds/4));
    this.walls.push(base[1].copy().translate(bounds/4, bounds/4));
    this.walls.push(base[0].copy().rotate(90).translate(bounds*3/4, bounds/4));
    this.walls.push(base[1].copy().rotate(90).translate(bounds*3/4, bounds/4));
    this.walls.push(base[0].copy().rotate(270).translate(bounds/4, bounds*3/4));
    this.walls.push(base[1].copy().rotate(270).translate(bounds/4, bounds*3/4));
    this.walls.push(base[0].copy().rotate(180).translate(bounds*3/4, bounds*3/4));
    this.walls.push(base[1].copy().rotate(180).translate(bounds*3/4, bounds*3/4));


    this.walls.push({x:bounds/4, y:bounds/4, w:thickness, h:bounds/6});
    this.walls.push({x:bounds/4+thickness, y:bounds/4, w:bounds/6, h:thickness});

    this.walls.push({x:bounds/4, y:bounds/4, w:thickness, h:bounds/6});
    this.walls.push({x:bounds/4+thickness, y:bounds/4, w:bounds/6, h:thickness});

    this.walls.push({x:bounds/4, y:bounds/4, w:thickness, h:bounds/6});
    this.walls.push({x:bounds/4+thickness, y:bounds/4, w:bounds/6, h:thickness});

};

GameEngine.prototype.placeFortresses = function (n, tries) {
    var bounds = this.config.mapSize * this.config.mapCellSize;
    var pad = 250;
    var dist = (bounds-pad*2)/2;

    this.fortresses = new Array(9);
    for (let x=0; x<3; x++) {
        for (let y=0; y<3; y++) {
            this.fortresses[y*3+x] = {alive: true,
                                      position: {x: pad + x * dist, y: pad + y * dist},
                                      angle: 0,
                                      playerTarget: null,
                                      missileTarget: null,
                                      radius: this.config.fortress.bigHex,
                                      config: this.config.fortress,
                                      respawnTimer: 0};
        }
    }
};

GameEngine.prototype.placeFortressesRandomly = function (n, tries) {
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
        for (let j=0; j<this.startLocations.length; j++) {
            if (distance(this.startLoctaions[j], pos) < (this.config.fortress.bigHex + (this.startLoctaions[j].w+this.startLoctaions[j].h)/2)) {
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
    this.events.length = 0;
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
};

GameEngine.prototype.getKeyName = function (key) {
    var action;
    var k;
    if (key[0]) action = 'press';
    else action = 'release';
    if (key[1] === KEY_LEFT) k = 'left';
    else if (key[1] === KEY_RIGHT) k = 'right';
    else if (key[1] === KEY_UP) k = 'thrust';
    else if (key[1] === KEY_SPACE) k = 'fire';
    else if (key[1] === KEY_1) k = '1';
    else if (key[1] === KEY_2) k = '2';
    else if (key[1] === KEY_3) k = '3';
    else if (key[1] === KEY_4) k = '4';

    return action + '-' + k;
};

GameEngine.prototype.processPlayerKeys = function (p, keys) {
    p.missileRequests = 0;
    p.messageRequests = [];
    for (var i=0; i<keys.length; i++) {
        this.addEvent({tag: this.getKeyName(keys[i]),
                       player: this.getPlayerIndex(p)});
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
            this.addEvent({tag: 'missile-fired', player: this.getPlayerIndex(p)});
        }
        for (let i=0; i<p.messageRequests.length; i++) {
            this.say(p, p.messageRequests[i]);
            this.addEvent({tag: 'player-transmission',
                           player: this.getPlayerIndex(p),
                           message: p.messageRequests[i]});
        }
        p.messageRequests.length = 0;

        if (p.position.x < 0 ||
            p.position.x > this.config.mapSize * this.config.mapCellSize ||
            p.position.y < 0 ||
            p.position.y > this.config.mapSize * this.config.mapCellSize) {
            this.killPlayer(p);
            this.addEvent({tag: 'player-hit-map-edge',
                           player: this.getPlayerIndex(p)});
        }
        for (let i=0; i<this.fortresses.length; i++) {
            if (this.fortresses[i].alive &&
                distance(this.fortresses[i].position, p.position) < this.config.player.collisionRadius+this.config.fortress.collisionRadius) {
                this.killPlayer(p);
                this.addEvent({tag: 'player-hit-fortress',
                               player: this.getPlayerIndex(p),
                               fortress: i});
                break;
            }
        }
        // console.log(p);
    } else {
        p.spawnTimer += 1;
        if (p.spawnTimer >= this.config.player.deathTimer) {
            p.alive = true;
            // FIXME: store start location in player object?
            var loc = this.startLocations[this.getPlayerIndex(p)];
            p.position.x = loc.x;
            p.position.y = loc.y;
            p.velocity.x = this.config.player.startVelocity.x;
            p.velocity.y = this.config.player.startVelocity.y;
            p.angle = this.config.player.startAngle;
            this.addEvent({tag:'player-respawn',
                           player: this.getPlayerIndex(p)});
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
        this.addEvent({tag:'drop-missile-target',
                       fortress: i,
                       missile: this.getMissileIndex(m)});
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
                this.addEvent({tag:'despawn-missile', missile: i});
                continue;
            }
            if (this.ticks - m.spawnTick > this.config.missile.lifespan) {
                this.killMissile(m);
                this.addEvent({tag:'despawn-missile', missile: i});
                continue;
            }
            for (let j=0; j<this.fortresses.length; j++) {
                var f = this.fortresses[j];
                if (f.alive && distance(f.position, m.position) < this.config.fortress.smallHex) {
                    // if (f.alive && this.hexagons[this.config.fortress.smallHex].inside(f.position, m.position)) {
                    this.killMissile(m);
                    var to = angleTo(f.position, m.position);
                    var a = angle_diff(f.angle, to);
                    if (a > 120 || a < -120) {
                        this.reward(this.config.rewards.fortressDestroy);
                        f.alive = false;
                        f.respawnTimer = 0;
                        this.addEvent({tag:'fortress-destroyed', fortress: j});
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
            this.addEvent({tag:'shell-despawn', shell: i});
            continue;
        }
        if (this.ticks - s.spawnTick > this.config.shell.lifespan) {
            s.alive = false;
            this.addEvent({tag:'shell-despawn', shell: i});
            continue;
        }
        for (let j=0; j<this.players.length; j++) {
            if (distance(s.position, this.players[j].position) < this.config.player.collisionRadius+this.config.shell.collisionRadius) {
                this.killPlayer(this.players[j]);
                s.alive = false;
                this.addEvent({tag:'shell-hit-player',
                               player: j,
                               shell: i});
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
            if (!f.playerTarget.alive ||
                // !this.hexagons[f.radius].inside(f.position, this.players[f.playerTarget].position)) {
                distance(f.position, f.playerTarget.position) > f.radius) {
                this.addEvent({tag: 'drop-player-target',
                               fortress: this.getFortressIndex(f),
                               target: this.getPlayerIndex(f.playerTarget)});
                f.playerTarget = null;
            }
        }
        if (f.missileTarget) {
            if (!f.missileTarget.alive ||
                // !this.hexagons[f.radius].inside(f.position, this.missileTarget.position)) {
                distance(f.position, f.missileTarget.position) > f.radius) {
                this.addEvent({tag: 'drop-missile-target',
                               fortress: this.getFortressIndex(f),
                               target: this.getMissileIndex(f.missileTarget)});
                f.missileTarget = null;
            }
        }
        if (!f.playerTarget) {
            for (i=0;i<this.players.length;i++) {
                if (this.players[i].alive &&
                    // this.hexagons[f.radius].inside(f.position, this.players[k].position)) {
                    distance(f.position, this.players[i].position) <= f.radius) {
                    f.playerTarget = this.players[i];
                    f.targetTimer = 0;
                    this.addEvent({tag: 'acquire-player-target',
                                   fortress: this.getFortressIndex(f),
                                   target: this.getPlayerIndex(f.playerTarget)});
                    break;
                }
            }
        }
        if (!f.playerTarget) {
            for (let i=0; i<this.missiles.length; i++) {
                // if (this.hexagons[f.radius].inside(f.position, this.missiles[i].position)) {
                if (distance(f.position, this.missiles[i].position) <= f.radius) {
                    f.missileTarget = this.missiles[i];
                    this.addEvent({tag: 'acquire-missile-target',
                                   fortress: this.getFortressIndex(f),
                                   target: this.getMissileIndex(f.missileTarget)});
                    break;
                }
            }
        }

        var target = null;
        if (f.playerTarget) target = f.playerTarget;
        else if (f.missileTarget) target = f.missileTarget;

        if (f.playerTarget) {
            // f.angle = angleTo(f.position, this.players[f.playerTarget].position);
            this.fortressAimAt(f, f.playerTarget.position);
            f.targetTimer += 1;
            if (f.targetTimer > this.config.fortress.lockTime) {
                this.addShell(f);
                f.targetTimer = 0;
                this.addEvent({tag:'shell-fired',
                               fortress: this.getFortressIndex(f),
                               target: this.getPlayerIndex(f.playerTarget)});
            }
        } else if (f.missileTarget) {
            this.fortressAimAt(f, f.missileTarget.position);
        } else {
            f.angle = stdAngle(f.angle+0.25);
        }
    } else {
        f.respawnTimer += 1;
        if (f.respawnTimer >= this.config.fortress.respawnTime) {
            f.alive = true;
            this.addEvent({tag:'fortress-respawn',
                           fortress: this.getFortressIndex(f)});
        }
    }
};

GameEngine.prototype.updatePlayers = function () {
    for (let i=0; i<this.players.length; i++) {
        var p = this.players[i];
        for (let j=0; j<p.movementRequests.length; j++) {
            p.lastMovementRequest = p.movementRequests[j][0];
            p.thrustFlag = p.movementRequests[j][1];
            p.turnFlag = p.movementRequests[j][2];
            p.missileRequests = p.movementRequests[j][3];
            p.messageRequests = p.movementRequests[j][4];
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
                    this.addEvent({tag:'missile-hit-asteroid',
                                   missile: j,
                                   asteroid: i});
                }
            }
            for (let j=0; j<this.players.length; j++) {
                if (this.players[j].alive) {
                    if (distance(this.players[j].position, pos) < this.asteroids[i].bubbles[b].r) {
                        this.killPlayer(this.players[j]);
                        this.addEvent({tag:'player-hit-asteroid',
                                       player: j,
                                       asteroid: i});
                    }
                }
            }
        }
    }
};

GameEngine.prototype.addPlayer = function (id) {
    var loc = this.startLocations[this.players.length % this.startLocations.length];
    this.players.push({id: id,
                       angle: this.config.player.startAngle,
                       position: {x: loc.x,
                                  y: loc.y},
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
                       color: this.config.player.colors[this.players.length%this.config.player.colors.length],
                       lastMovementRequest: -1,
                       movementRequests: []
                      });
};

GameEngine.prototype.delPlayer = function (id) {
    var match = null;
    for (let i=0; i<this.players.length;i++) {
        if (this.players[i].id === id) {
            match = this.players[i];
            break;
        }
    }

    for (let i=0; i<this.fortresses.length; i++) {
        if (this.fortresses[i].playerTarget === match)
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
    var titles = new Array(8);

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
    titles[5] = ['missile_angle',
                 'missile_x',
                 'missile_y'];
    titles[6] = ['shell_angle',
                 'shell_x',
                 'shell_y'];
    titles[7] = 'events';
    return titles;
};

Number.prototype.fixed = function(n) { n = n || 3; return parseFloat(this.toFixed(n)); };

GameEngine.prototype.dumpState = function () {
    var numPlayers = 2;
    var state = new Array(8);

    state[0] = this.ticks;
    state[1] = this.clock;

    state[2] = new Array(numPlayers);
    for (let i=0;i<this.players.length;i++) {
        state[2][i] = [this.players[i].alive?1:0,
                       this.players[i].angle.fixed(1),
                       this.players[i].position.x.fixed(3),
                       this.players[i].position.y.fixed(3),
                       this.players[i].velocity.x.fixed(3),
                       this.players[i].velocity.y.fixed(3),
                       this.players[i].turnFlag,
                       this.players[i].thrustFlag?1:0];
    }

    state[3] = new Array(this.fortresses.length);
    for (let i=0; i<this.fortresses.length; i++) {
        state[3][i] = [this.fortresses[i].alive?1:0,
                       this.fortresses[i].angle.fixed(1),
                       this.getPlayerIndex(this.fortresses[i].playerTarget),
                       this.getMissileIndex(this.fortresses[i].missileTarget)];
    }

    state[4] = new Array(this.asteroids.length);
    for (let i=0; i<this.asteroids.length; i++) {
        state[4][i] = [this.asteroids[i].angle.fixed(1),
                       this.asteroids[i].position.x.fixed(3),
                       this.asteroids[i].position.y.fixed(3),
                       this.asteroids[i].velocity.x.fixed(3),
                       this.asteroids[i].velocity.y.fixed(3)];
    }
    state[5] = new Array(this.missiles.length);
    for (let i=0; i<this.missiles.length;i++) {
        state[5][i] = [this.missiles[i].angle.fixed(1),
                       this.missiles[i].position.x.fixed(3),
                       this.missiles[i].position.y.fixed(3)];
    }
    state[6] = new Array(this.shells.length);
    for (let i=0; i<this.shells.length;i++) {
        state[6][i] = [this.shells[i].angle.fixed(1),
                       this.shells[i].position.x.fixed(3),
                       this.shells[i].position.y.fixed(3)];
    }

    state[7] = this.events.slice();

    return state;
};

exports.GameEngine = GameEngine;
