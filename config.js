function Config() {
    this.network = { maxDist: 10,
                     maxAngle: 10
                   };
    this.player = {startPosition: {x: 1500.0, y: 1500.0},
                   startVelocity: {x:0.0, y:0.0},
                   startAngle: 100,
                   turnRate: 3,
                   // acceleration: 0.02,
                   acceleration: 0.04,
                   maxSpeed: 5,
                   collisionRadius: 10,
                   deathTimer: 120,
                   health: 500,
                   maxWater: 50,
                   waterFillRate: 1};
    this.fortress = { lockTime: 120,
                      smallHex: 40,
                      bigHex: 170,
                      collisionRadius: 18,
                      speed: 0.25,
                      lockSpeed: 5};
    this.missile = { collisionRadius: 5,
                     speed: 3,
                     lifespan: 90};
    this.shell = { collisionRadius: 3,
                   speed: 1.5,
                   lifespan: 180};
    this.smoke = { duration: 1200 }; // 800
    this.retardant = { duration: 70,
                       durJitter: 40};

    this.map = { resizeDuration: 200, // 30 * 60,
                 resizeAmount: 2,
                 cellSize: 20,
                 startFires: 30 };
    this.fire = {spreadingOdds: 0.7,
                 ticks: 2};

    this.serverUpdateBufferSize = 60;
}

Config.prototype = {};

exports.Config = Config;
