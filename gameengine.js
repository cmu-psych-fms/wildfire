var KEY_LEFT = 1;
var KEY_RIGHT = 2;
var KEY_UP = 3;
var KEY_DOWN = 4;
var KEY_SPACE = 5;

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
    this.ticks = 0;

    this.hexagons = {};
    this.hexagons[150] = new Hexagon(150);
    this.hexagons[40] = new Hexagon(40);
}

GameEngine.prototype = {};

GameEngine.prototype.placeFortresses = function (n, tries) {
    this.fortresses = [];
    console.log('start placing', this.fortresses.length);
    for (let i=0; i<tries; i++) {
        var pos = {x: Math.random()*this.config.mapSize*20,
                   y: Math.random()*this.config.mapSize*20};
        var ok = true;
        for (let j=0; j<this.fortresses.length; j++) {
            if (distance(this.fortresses[j].position, pos) < 150*2) {
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
                                  radius: 150,
                                  config: this.config.fortress
                                 });
        if (this.fortresses.length >= n) break;
    }
    console.log('done placing', this.fortresses.length);
};

GameEngine.prototype.createMap = function () {
    this.map = Array(this.config.mapSize * this.config.mapSize);
};

GameEngine.prototype.stepOneTick = function () {
    this.ticks += 1;
    this.updatePlayers();
    this.updateFortresses();
    this.updateMissiles();
    this.updateShells();
    // this.updateEntities();
    // this.handleCollisions();
};

GameEngine.prototype.processPlayerKeys = function (p, keys) {
    for (var i=0; i<keys.length; i++) {
        // console.log(keys[i][0] ? 'pressed':'released', keys[i][1]);
        if (keys[i][0] === 1) {
            if (keys[i][1] === KEY_LEFT) p.turnFlag = 'left';
            else if (keys[i][1] === KEY_RIGHT) p.turnFlag = 'right';
            else if (keys[i][1] === KEY_UP) p.thrustFlag = true;
            else if (keys[i][1] === KEY_SPACE) this.addMissile(p);
        } else {
            if (keys[i][1] === KEY_LEFT || keys[i][1] === KEY_RIGHT) p.turnFlag = null;
            else if (keys[i][1] === KEY_UP) p.thrustFlag = false;
            else if (keys[i][1] === KEY_SPACE) p.missileState = false;
        }
    }
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
            if (f.alive && distance(f.position, m.position) < 40) {
            // if (f.alive && this.hexagons[40].inside(f.position, m.position)) {
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

GameEngine.prototype.addPlayer = function (id) {
    this.players[id] = {id: id,
                        angle: this.config.player.startAngle,
                        position: {x: this.config.player.startPosition.x,
                                   y: this.config.player.startPosition.y},
                        velocity: {x: this.config.player.startVelocity.x,
                                   y: this.config.player.startVelocity.y},
                        config: this.config.player,
                        turnFlag: 0,
                        thrustFlag: 0,
                        missileState: 0,
                        spawnTimer: 0,
                        alive: true
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
};


exports.GameEngine = GameEngine;
