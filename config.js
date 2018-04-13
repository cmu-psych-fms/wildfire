function Config() {
    this.player = {startPosition: {x: 500.0, y: 500.0},
                   startVelocity: {x:0.0, y:0.0},
                   startAngle: 0,
                   turnRate: 3,
                   // acceleration: 0.02,
                   acceleration: 0.04,
                   maxSpeed: 3,
                   collisionRadius: 10,
                   deathTimer: 120};
    this.fortress = { lockTime: 120,
                      smallHex: 40,
                      bigHex: 170,
                      collisionRadius: 18,
                      speed: 0.25,
                      lockSpeed: 5,
                      respawnTime: 30*60};
    this.missile = { collisionRadius: 5,
                     speed: 3,
                     lifespan: 90};
    this.shell = { collisionRadius: 3,
                   speed: 1.5,
                   lifespan: 180};
    this.mapSize = 100;
    this.mapCellSize = 20;
    this.serverUpdateBufferSize = 60;
}

Config.prototype = {};

exports.Config = Config;
