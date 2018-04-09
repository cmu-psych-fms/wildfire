function Config() {
    this.player = {startPosition: {x: 10.0, y: 10.0},
                   startVelocity: {x:0.0, y:0.0},
                   startAngle: 0,
                   turnRate: 3,
                   acceleration: 0.02,
                   maxSpeed: 8};
    this.mapSize = 100;
    this.serverUpdateBufferSize = 60;
}

Config.prototype = {};

exports.Config = Config;
