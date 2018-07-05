//Code below is from Three.js, and sourced from links below

    // http://paulirish.com/2011/requestanimationframe-for-smart-animating/
    // http://my.opera.com/emoller/blog/2011/12/20/requestanimationframe-for-smart-er-animating

    // requestAnimationFrame polyfill by Erik Möller
    // fixes from Paul Irish and Tino Zijdel

( function () {
    var frame_time = 60/1000;
    var lastTime = 0;
    var vendors = [ 'ms', 'moz', 'webkit', 'o' ];

    for ( var x = 0; x < vendors.length && !window.requestAnimationFrame; ++ x ) {
        window.requestAnimationFrame = window[ vendors[ x ] + 'RequestAnimationFrame' ];
        window.cancelAnimationFrame = window[ vendors[ x ] + 'CancelAnimationFrame' ] || window[ vendors[ x ] + 'CancelRequestAnimationFrame' ];
    }

    if ( !window.requestAnimationFrame ) {
        window.requestAnimationFrame = function ( callback, element ) {
            var currTime = Date.now(), timeToCall = Math.max( 0, frame_time - ( currTime - lastTime ) );
            var id = window.setTimeout( function() { callback( currTime + timeToCall ); }, timeToCall );
            lastTime = currTime + timeToCall;
            return id;
        };
    }

    if ( !window.cancelAnimationFrame ) {
        window.cancelAnimationFrame = function ( id ) { clearTimeout( id ); };
    }

}() );

function Cache(scene, geom, material, z) {
    this.scene = scene;
    this.cache = [];
    this.geom = geom;
    this.material = material;
    this.z = z || 35;
}

Cache.prototype = {};

Cache.prototype.clear = function () {
    for (i=0; i<this.cache.length; i++) {
        this.cache[i].visible = false;
    }
};

Cache.prototype.add = function (thing) {
    var found = -1;
    for (i=0; i<this.cache.length; i++) {
        if (!this.cache[i].visible) {
            // found = i;
            break;
        }
    }
    if (found < 0) {
        var m = new THREE.Mesh(this.geom, this.material);
        this.scene.add(m);
        this.cache.push(m);
        found = this.cache.length-1;
    }

    this.cache[found].position.x = thing.position.x;
    this.cache[found].position.y = thing.position.y;
    this.cache[found].position.z = this.z;
    this.cache[found].visible = true;
    thing.mesh = this.cache[found];
};

function SphereCache(scene, geom, material, chaseMaterial) {
    this.roamCache = new Cache(scene, geom, material);
    this.chaseCache = new Cache(scene, geom, chaseMaterial);
}

SphereCache.prototype = {};

SphereCache.prototype.clear = function () {
    this.roamCache.clear();
    this.chaseCache.clear();
};


SphereCache.prototype.add = function (s) {
    if (s.target !== null) this.chaseCache.add(s);
    else this.roamCache.add(s);
};


function WebClient (gnum) {
    this.game_number = gnum;
    this.engine = new GameEngine(new Config());
    this.network = {serverUpdates: [],
                    latency: 0};
    this.id = null;
    this.keyEvents = [];
    this.movementRequests = [];
    this.movementRequestSeq = -1;
    this.gameStates = [];
    this.lastUpdateTime = null;
    this.leftOverDT = 0;

    this.messageText = ["Impossible",
                        "I need help. Come to my position.",
                        "Split up to cover more ground.",
                        "Follow me.",
                        "10-4. In transit."];
}

WebClient.prototype = {};

WebClient.prototype.setCameraMode = function (mode) {
    if (this.cameraMode === '2d' && mode !== '2d') {
        this.canvas2d.style.display = 'none';
        this.canvas3d.style.display = 'inline-block';
    }
    this.cameraMode = mode;
    if (mode === '2d') {
        this.canvas2d.style.display = 'inline-block';
        this.canvas3d.style.display = 'none';
    }
};

WebClient.prototype.init = function () {
    $("#experiment_area").html('<div style="text-align: center">'+
                               '<canvas id="canvas2d" style="background: #000000"></canvas>'+
                               '<canvas id="canvas3d" style="position:absolute; top:0; left:0"></canvas>'+
                               '<div id="points"></div>'+
                               '</div>'+
                               '<div id="status_area" class="message-container"><div class="message-body"><div id="status"></div></div></div>');
                               // '<div style="position: absolute; top: 10px; left: 10px">'+
                               // '<table>'+
                               // '<tr><td>Camera Modes</td>'+
                               // '<tr><td><button onclick="exp.currentScreen().setCameraMode("2d")">2D Mode</button></td>'+
                               // '<tr><td><button onclick="exp.currentScreen().setCameraMode("chase")">Chase Camera</button></td>'+
                               // '<tr><td><button onclick="exp.currentScreen().setCameraMode("overhead")">Overhead Camera</button></td>'+
                               // '<tr><td><button onclick="exp.currentScreen().setCameraMode("stationary")">Stationary Camera</button></td>'+
                               // '<tr><td><button onclick="exp.currentScreen().setCameraMode("muzzle")">Muzzle Camera</button></td>'+
                               // '</table>'+
                               // '</div>'+
                               // '<div style="position: absolute; top: 10px; right: 10px">'+
                               // '<table>'+
                               // '<tr><td>1<td>I need help. Come to my position</tr>'+
                               // '<tr><td>2<td>Split up to cover more ground</tr>'+
                               // '<tr><td>3<td>Follow me</tr>'+
                               // '<tr><td>4<td>10-4. In transit.</tr>'+
                               // '</table></div>');


    this.connect();
    this.addEventListeners();

    this.canvas2d = document.getElementById('canvas2d');
    this.canvas3d = document.getElementById('canvas3d');
    this.canvas2d.width = 600;
    this.canvas2d.height = 500;
    this.ctx = this.canvas2d.getContext('2d');

    this.cameraMode = 'chase';
    this.scene = new THREE.Scene();
    this.camera = new THREE.PerspectiveCamera(70,
                                              // this.canvas2d.clientWidth / this.canvas2d.clientHeight,
                                              window.innerWidth/window.innerHeight,
                                              1,
                                              10000);
    this.camera.up.set(0,0,1);

    this.scene.add(this.camera);

    this.scene.background = new THREE.Color( 0xf0f0f0 );

    var ambient = new THREE.AmbientLight( 0xffffff, 0.2 );
    this.scene.add(ambient);

    var light = new THREE.DirectionalLight( 0xffffff, 1 );
    light.position.set( 0.75, -1, 1 ).normalize();
    this.scene.add( light );

    this.renderer = new THREE.WebGLRenderer({canvas:this.canvas3d});
    this.renderer.setSize(window.innerWidth, window.innerHeight);

    this.canvas2d.style.display = 'none';
    this.canvas3d.style.display = 'none';

    $('#status').html('<h1>Connecting to game server ...</h1>');
};

WebClient.prototype.addLobbyEventListeners = function () {
    $(document).on('keydown', $.proxy(this.lobbyOnKeyDown, this));
    $(document).on('keyup', $.proxy(this.lobbyOnKeyUp, this));
};


WebClient.prototype.addEventListeners = function () {
    $(document).on('keydown', $.proxy(this.onKeyDown, this));
    $(document).on('keyup', $.proxy(this.onKeyUp, this));
};

WebClient.prototype.clearEvents = function () {
    $(document).off('keydown', $.proxy(this.lobbyOnKeyDown));
    $(document).off('keyup', $.proxy(this.lobbyOnKeyUp));
    $(document).off('keydown', $.proxy(this.onKeyDown));
    $(document).off('keyup', $.proxy(this.onKeyUp));
};

WebClient.prototype.cleanup = function () {
    this.engine.players = [];
    this.clearEvents();
    exp.gameReward = this.engine.config.pointConversion * this.engine.points;
    exp.gamePoints = this.engine.points;

    exp.lg('end');
    this.cancelUpdates();
};

WebClient.prototype.lobbyOnKeyDown = function (ev) {
    
};

WebClient.prototype.lobbyOnKeyUp = function (ev) {
    
};

WebClient.prototype.connect = function () {
    this.network.socket = io.connect();
    this.network.socket.on('connect', this.onConnect.bind(this));
    this.network.socket.on('joined', this.onJoined.bind(this));

    this.network.socket.on('disconnect', this.onDisconnect.bind(this));
    this.network.socket.on('message', this.onMessage.bind(this));
    this.network.socket.on('serverupdate', this.onServerUpdate.bind(this));
    this.network.socket.on('join', this.onPlayerJoin.bind(this));
    this.network.socket.on('part', this.onPlayerPart.bind(this));
    this.network.socket.on('end', this.onEndGame.bind(this));
    this.network.socket.on('starting', this.onStarting.bind(this));
};


WebClient.prototype.decodeKeyCode = function(which) {
    // FIXME: oops. The 3d coordinates are mirror image of 2d coordinates.
    if (which === 65 || which === 37)
        return this.cameraMode === '2d'? KEY_LEFT:KEY_RIGHT;
    else if (which === 68 || which === 39)
        return this.cameraMode === '2d'? KEY_RIGHT:KEY_LEFT;
    else if (which === 87 || which === 38)
        return KEY_UP;
    else if (which === 83 || which === 40)
        return KEY_DOWN;
    else if (which === 32)
        return KEY_SPACE;
    else if (which === 49)
        return KEY_1;
    else if (which === 50)
        return KEY_2;
    else if (which === 51)
        return KEY_3;
    else if (which === 52)
        return KEY_4;
    else
        return undefined;
};

WebClient.prototype.cancelUpdates = function () {
    console.log("cancel animation");
    window.cancelAnimationFrame(this.updateid);
    this.updateid = null;
    this.network.socket.close();
};

WebClient.prototype.onKeyDown = function (ev) {
    if (ev.which === 27) this.cancelUpdates();
    var k = this.decodeKeyCode(ev.which);
    if (k) {
        this.network.hasNewInput = true;
        this.keyEvents.push([1, k]);
        ev.preventDefault();
        ev.stopPropagation();
    }
};

WebClient.prototype.onKeyUp = function (ev) {
    var k = this.decodeKeyCode(ev.which);
    if (k) {
        this.network.hasNewInput = true;
        this.keyEvents.push([0, k]);
        ev.preventDefault();
        ev.stopPropagation();
    }
};

WebClient.prototype.updateScene = function () {
    for (let i=0; i<this.engine.players.length; i++) {
        this.engine.players[i].mesh.position.x = this.engine.players[i].position.x;
        this.engine.players[i].mesh.position.y = this.engine.players[i].position.y;
        // this.engine.players[i].mesh.position.z = 10;
        this.engine.players[i].mesh.rotation.z = this.engine.players[i].angle * Math.PI/180;
        this.engine.players[i].mesh.visible = this.engine.players[i].alive ? true:false;
    }
    for (let i=0; i<this.engine.fortresses.length; i++) {
        this.engine.fortresses[i].mesh.position.x = this.engine.fortresses[i].position.x;
        this.engine.fortresses[i].mesh.position.y = this.engine.fortresses[i].position.y;
        // this.engine.fortresses[i].mesh.position.z = 10;
        this.engine.fortresses[i].mesh.rotation.z = this.engine.fortresses[i].angle * Math.PI/180;
        this.engine.fortresses[i].mesh.visible = this.engine.fortresses[i].alive?true:false;
    }
    for (let i=0; i<this.engine.asteroids.length; i++) {
        this.engine.asteroids[i].group.position.x = this.engine.asteroids[i].position.x;
        this.engine.asteroids[i].group.position.y = this.engine.asteroids[i].position.y;
        this.engine.asteroids[i].group.rotation.z = this.engine.asteroids[i].angle * Math.PI/180;
    }
    for (let i=0; i<this.engine.shells.length; i++) {
        this.engine.shells[i].mesh.position.x = this.engine.shells[i].position.x;
        this.engine.shells[i].mesh.position.y = this.engine.shells[i].position.y;
        this.engine.shells[i].mesh.rotation.z = this.engine.shells[i].angle * Math.PI/180;
    }
    for (let i=0; i<this.engine.missiles.length; i++) {
        this.engine.missiles[i].mesh.position.x = this.engine.missiles[i].position.x;
        this.engine.missiles[i].mesh.position.y = this.engine.missiles[i].position.y;
        this.engine.missiles[i].mesh.rotation.z = this.engine.missiles[i].angle * Math.PI/180;
    }
};

WebClient.prototype.createFortressShield = function (radius) {
    var h = this.engine.hexagons[radius];
    var geometry = new THREE.Geometry();
    for (let i=0; i<h.points.length; i++)
        geometry.vertices.push( new THREE.Vector3(h.points[i].x, h.points[i].y, 0));
    geometry.vertices.push(
        new THREE.Vector3(h.points[0].x, h.points[0].y, 20),
        new THREE.Vector3(h.points[1].x, h.points[1].y, 20),
        new THREE.Vector3(h.points[2].x, h.points[2].y, 20),
        new THREE.Vector3(h.points[3].x, h.points[3].y, 20),
        new THREE.Vector3(h.points[4].x, h.points[4].y, 20),
        new THREE.Vector3(h.points[5].x, h.points[5].y, 20));

    for (let i=1; i<5; i++)
        geometry.faces.push(
            new THREE.Face3(i+6, i+7, i+0),
            new THREE.Face3(i+0, i+1, i+7));

    geometry.computeFaceNormals();

    return geometry;
}

WebClient.prototype.makeGrid = function () {
    var gridGeom = new THREE.BufferGeometry();
    var m = this.engine.config.mapSize * this.engine.config.mapCellSize;
    var step = this.engine.config.mapCellSize*2;
    var vertices = [];

    for (let i=0; i<m; i+=step) {
        vertices.push(i, 0, 1, i, m, 1);
        vertices.push(0, i, 1, m, i, 1);
    }
    gridGeom.addAttribute( 'position', new THREE.Float32BufferAttribute(vertices, 3));
    return new THREE.LineSegments(gridGeom, new THREE.LineBasicMaterial( { color: 0x770077}));
}

WebClient.prototype.addPlayerToScene = function (p) {
    material = new THREE.MeshLambertMaterial({color: p.color});
    p.mesh = new THREE.Group();
    p.mesh.add (new THREE.Mesh( new THREE.CubeGeometry(20,20,20,1,1,1),
                                material));
    var barrel = new THREE.Mesh( new THREE.CubeGeometry(20,5,5,1,1,1), material);
    barrel.position.x = 20;
    barrel.position.z = 5;
    p.mesh.add (barrel);

    p.mesh.position.z = 30;
    this.scene.add(p.mesh);
};

WebClient.prototype.addWorldToScene = function () {
    var platformMat = new THREE.MeshLambertMaterial({ color: 0x999999});
    var material = new THREE.MeshLambertMaterial({ color: 0x00FF00 });

    for (let k in this.engine.hexagons) {
        var hexShape = new THREE.Shape();
        hexShape.moveTo(this.engine.hexagons[k].points[0].x, this.engine.hexagons[k].points[0].y);
        for (let i=1; i<this.engine.hexagons[k].points.length; i++)
            hexShape.lineTo(this.engine.hexagons[k].points[i].x, this.engine.hexagons[k].points[i].y);
        // hexShape.lineTo(this.hexagons[k].points[0]);
        this.engine.hexagons[k].platformGeom = new THREE.ExtrudeGeometry( hexShape, {amount: 10, bevelEnabled: false } );
    }

    var shieldMat = new THREE.MeshLambertMaterial({color:0xFF00FF,
                                                   side: THREE.DoubleSide,
                                                   opacity: 0.4,
                                                   transparent: true});
    var shieldGeom = this.createFortressShield(this.engine.config.fortress.smallHex);

    for (let i=0; i<this.engine.fortresses.length; i++) {
        this.engine.fortresses[i].mesh = new THREE.Group();
        this.engine.fortresses[i].mesh.add( new THREE.Mesh( new THREE.CubeGeometry(20, 20, 20, 1, 1, 1),
                                                            material));
        var barrel = new THREE.Mesh( new THREE.CubeGeometry(20, 5, 5, 1, 1, 1),
                                     material);
        barrel.position.x = 20;
        barrel.position.z = 5;
        this.engine.fortresses[i].mesh.add(barrel);
        var shield = new THREE.Mesh(shieldGeom, shieldMat);
        this.engine.fortresses[i].mesh.add(shield);

        this.engine.fortresses[i].mesh.position.z = 30;
        this.scene.add(this.engine.fortresses[i].mesh);
        var m = new THREE.Mesh(this.engine.hexagons[this.engine.config.fortress.bigHex].platformGeom, platformMat);
        m.position.x = this.engine.fortresses[i].position.x;
        m.position.y = this.engine.fortresses[i].position.y;
        m.position.z = 0;
        this.scene.add(m);
    }

    for (let i=0; i<this.engine.players.length; i++) {
        this.addPlayerToScene(this.engine.players[i]);
    }

    for (let i=0; i<this.engine.startLocations.length; i++) {
        var mesh = new THREE.Mesh( new THREE.CubeGeometry(this.engine.startLocations[i].w, this.engine.startLocations[i].h, 5, 1, 1, 1),
                                   new THREE.MeshLambertMaterial({color: this.engine.startLocations[i].color}));
        mesh.position.x = this.engine.startLocations[i].x;
        mesh.position.y = this.engine.startLocations[i].y;
        this.scene.add(mesh);
    }

    material = new THREE.MeshPhongMaterial({color: 0x3333FF,
                                            emissive: 0x072534,
                                            flatShading: true});

    for (let i=0; i<this.engine.asteroids.length; i++) {
        this.engine.asteroids[i].group = new THREE.Group();
        for (let j=0; j<this.engine.asteroids[i].bubbles.length; j++) {
            var r = this.engine.asteroids[i].bubbles[j].r;
            var bm = new THREE.Mesh( new THREE.SphereGeometry(r, 8, 8), material);
            bm.position.x = this.engine.asteroids[i].bubbles[j].x;
            bm.position.y = this.engine.asteroids[i].bubbles[j].y;
            bm.position.z = 0;
            this.engine.asteroids[i].group.add(bm);
        }
        this.engine.asteroids[i].group.position.z = 0;
        this.scene.add(this.engine.asteroids[i].group);
    }

    var borderMat = new THREE.MeshLambertMaterial({color: 0xFF8800});
    this.three = {};


    this.three.missileCache = new Cache(this.scene,
                                        new THREE.CubeGeometry(20, 5, 5, 1, 1, 1),
                                        new THREE.MeshLambertMaterial({color: 0xFFFF00}),
                                        35);
    this.three.shellCache = new Cache(this.scene,
                                      new THREE.CubeGeometry(20, 5, 5, 1, 1, 1),
                                      new THREE.MeshLambertMaterial({color: 0xFF0000}),
                                      35);
    this.three.sphereCache = new SphereCache(this.scene,
                                             new THREE.SphereGeometry(this.engine.config.spheres.radius * 1.5, 8, 8),
                                             new THREE.MeshPhongMaterial({color: 0x3333FF,
                                                                          emissive: 0x072534,
                                                                          flatShading: true}),
                                             new THREE.MeshPhongMaterial({color: 0xFF3333,
                                                                          emissive: 0x072534,
                                                                          flatShading: true}),
                                             0.1);

    // Create a few objects for the cache to avoid lag later
    for (let i=0; i<10; i++) {
        this.three.shellCache.add({position: {x:0,y:0}});
        this.three.missileCache.add({position: {x:0,y:0}});
        this.three.sphereCache.add({position: {x:0,y:0}, target: null});
        this.three.sphereCache.add({position: {x:0,y:0}, target: 0});
    }
    this.three.shellCache.clear();
    this.three.missileCache.clear();
    this.three.sphereCache.clear();

    this.three.floor = new THREE.Mesh( new THREE.PlaneGeometry( this.engine.config.mapSize * this.engine.config.mapCellSize,
                                                                this.engine.config.mapSize * this.engine.config.mapCellSize,
                                                                this.engine.config.mapCellSize,this.engine.config.mapCellSize),
                                       new THREE.MeshBasicMaterial({color: 0x330033}));
    this.three.floor.position.x = this.engine.config.mapSize * this.engine.config.mapCellSize/2;
    this.three.floor.position.y = this.engine.config.mapSize * this.engine.config.mapCellSize/2;


    this.three.grid = this.makeGrid();
    this.scene.add(this.three.grid);

    this.scene.add(this.three.floor);

    this.three.walls = new Array(this.engine.walls.length);
    for (let i=0; i<this.engine.walls.length; i++) {
        this.three.walls[i] = new THREE.Mesh( new THREE.CubeGeometry( Math.abs(this.engine.walls[i].x1 - this.engine.walls[i].x2),
                                                                      Math.abs(this.engine.walls[i].y1 - this.engine.walls[i].y2),
                                                                      50, 1,1,1),
                                              borderMat);
        this.three.walls[i].position.x = this.engine.walls[i].x1 + (this.engine.walls[i].x2 - this.engine.walls[i].x1)/2;
        this.three.walls[i].position.y = this.engine.walls[i].y1 + (this.engine.walls[i].y2 - this.engine.walls[i].y1)/2;
        this.scene.add(this.three.walls[i]);
    }

    // this.three.topBorder = new THREE.Mesh( new THREE.CubeGeometry( this.engine.config.mapSize * this.engine.config.mapCellSize, 5, 50,
    //                                                                1,1,1),
    //                                     borderMat);
    // this.three.bottomBorder = new THREE.Mesh( new THREE.CubeGeometry( this.engine.config.mapSize * this.engine.config.mapCellSize, 5, 50,
    //                                                                   1,1,1),
    //                                        borderMat);
    // this.three.leftBorder = new THREE.Mesh( new THREE.CubeGeometry( 5, this.engine.config.mapSize * this.engine.config.mapCellSize, 50,
    //                                                              1,1,1),
    //                                        borderMat);
    // this.three.rightBorder = new THREE.Mesh( new THREE.CubeGeometry( 5, this.engine.config.mapSize * this.engine.config.mapCellSize, 50,
    //                                                              1,1,1),
    //                                        borderMat);

    // this.three.topBorder.position.x = this.engine.config.mapSize * this.engine.config.mapCellSize/2;
    // this.three.bottomBorder.position.x = this.engine.config.mapSize * this.engine.config.mapCellSize/2;
    // this.three.bottomBorder.position.y = this.engine.config.mapSize * this.engine.config.mapCellSize;
    // this.three.leftBorder.position.y = this.engine.config.mapSize * this.engine.config.mapCellSize/2;
    // this.three.rightBorder.position.y = this.engine.config.mapSize * this.engine.config.mapCellSize/2;
    // this.three.rightBorder.position.x = this.engine.config.mapSize * this.engine.config.mapCellSize;

    // this.scene.add(this.three.topBorder);
    // this.scene.add(this.three.bottomBorder);
    // this.scene.add(this.three.leftBorder);
    // this.scene.add(this.three.rightBorder);
};

WebClient.prototype.onConnect = function () {
    console.log('connect');
    this.network.socket.send('i' + JSON.stringify({id: getWorkerId(), gnum: this.game_number}));
    $('#status').html('<h1>Identifying Client ...</h1>');
};

WebClient.prototype.onJoined = function (data) {
    console.log('joined', data);
    this.id = data.id;
    this.engine.startLocations = data.startLocations;
    for (let i=0; i<data.players.length; i++) {
        this.engine.addPlayer(data.players[i].id);
        this.engine.players[i].color = data.players[i].color;
    }
    this.engine.fortresses = new Array(data.fortresses.length);
    for (let i=0; i<data.fortresses.length; i++) {
        this.engine.fortresses[i] = {alive: data.fortresses[i][0],
                                     position: {x:data.fortresses[i][1],
                                                y:data.fortresses[i][2]},
                                     angle: data.fortresses[i][3],
                                     radius: data.fortresses[i][4]};
    }
    this.engine.asteroids = data.asteroids;
    this.engine.walls = data.walls;
    this.engine.spheres = data.spheres;

    this.addWorldToScene();

    $('#status').html('<h1>Waiting For Other Player ...</h1>');
};

WebClient.prototype.onStarting = function (data) {
    $('#status').html('<h1>Game will start in</h1><span style="font-size: 50px; font-weight: bold">'+data.seconds.toString()+'</span>');
};

WebClient.prototype.onDisconnect = function (data) {
    // We got disconnected
    console.log('disconnect', data);
    this.network.state = 'disconnected';
    exp.nextScreen();
};


WebClient.prototype.sendPing = function () {
    var ts = new Date().getTime();
    this.network.socket.send('p'+ts.toString());
};

WebClient.prototype.handlePong = function (ts) {
    this.network.latency = new Date().getTime() - ts;
    console.log('latency is ', this.network.latency);
};

WebClient.prototype.onMessage = function (msg) {
    console.log('message', msg);
    var cmd = msg[0];
    var data = JSON.parse(msg.slice(1));
    switch (cmd) {
    case 'p':
        this.handlePong(data);
    };
};

WebClient.prototype.onPlayerJoin = function (data) {
    console.log('join', data);
    this.engine.addPlayer(data.id);
    this.addPlayerToScene(this.engine.getPlayer(data.id));
    $('#status').html('<h1>Game will start in 5 seconds!</h1>');
};

WebClient.prototype.onPlayerPart = function (data) {
    console.log('part', data);
    this.scene.remove(this.engine.getPlayer(data.id).mesh);
    this.engine.delPlayer(data.id);
};

WebClient.prototype.onEndGame = function (data) {
    console.log('end game');
    exp.nextScreen();
};

WebClient.prototype.onServerUpdate = function (data) {
    this.network.serverUpdates.push(data);
    if (this.network.serverUpdates.length >= this.engine.config.serverUpdateBufferSize) {
        this.network.serverUpdates.splice(0,1);
    }
    if (!this.updateid) {
        $('#status_area').css('display', 'none');
        this.canvas3d.style.display = 'inline-block';
        this.requestUpdate();
    }
};

WebClient.prototype.processKbdInput = function () {
    var p = this.engine.getPlayer(this.id);
    this.engine.processPlayerKeys(p, this.keyEvents);
    this.movementRequestSeq += 1;
    // console.log('master yah',
    //             this.engine.players[this.id].missileRequests,
    //             this.engine.players[this.id].messageRequests);
    var m = [this.movementRequestSeq,
             p.thrustFlag,
             p.turnFlag,
             p.missileRequests,
             p.messageRequests];
    var packet = 'k' + JSON.stringify(m);
    this.network.socket.send (packet);
    this.movementRequests.push(m);
    this.keyEvents = [];
    if (p.alive)
        this.engine.applyPlayerMovements(p);
};

WebClient.prototype.replayMovementRequests = function () {
    var p = this.engine.getPlayer(this.id);

    if (p.alive) {
        for (let i=0; i<this.movementRequests.length; i++) {
            p.thrustFlag = this.movementRequests[i][1];
            p.turnFlag = this.movementRequests[i][2];
            p.missileRequests = this.movementRequests[i][3];
            this.engine.applyPlayerMovements(p);
        }
    }
};

WebClient.prototype.lerpThing = function (thing) {
    if (thing.lerp.step >= thing.lerp.steps) return;

    thing.lerp.step += 1;
    thing.position.x += thing.lerp.dx;
    thing.position.y += thing.lerp.dy;
    thing.angle += thing.lerp.da;
};

WebClient.prototype.calcLerp = function (thing, start, end, ticks) {
    thing.lerp = { step: 0,
                   steps: ticks,
                   start: start,
                   end: end,
                   dx: (end.x - start.x)/ticks,
                   dy: (end.y - start.y)/ticks,
                   da: (end.angle - start.angle)/ticks}
};

WebClient.prototype.predictPlayer = function (p) {
    if (p.alive) {
        if (p.turnFlag === 'left') {
            p.angle -= p.config.turnRate;
        } else if (p.turnFlag === 'right') {
            p.angle += p.config.turnRate;
        }
        p.angle = stdAngle(p.angle);

        p.position.x += p.velocity.x;
        p.position.y += p.velocity.y;
    }
};

WebClient.prototype.predictMissile = function (m) {
    m.position.x += m.velocity.x;
    m.position.y += m.velocity.y;
};

WebClient.prototype.predictShell = function (s) {
    s.position.x += this.engine.config.shell.speed * Math.cos(deg2rad(s.angle));
    s.position.y += this.engine.config.shell.speed * Math.sin(deg2rad(s.angle));
};

WebClient.prototype.predictAsteroid = function (a) {
    a.position.x += a.velocity.x;
    a.position.y += a.velocity.y;
    a.angle = stdAngle(a.angle+a.angularVelocity);
};

WebClient.prototype.predictSphere = function (s) {
    s.position.x += s.velocity.x;
    s.position.y += s.velocity.y;
};

WebClient.prototype.predictiveStep = function () {
    for (let i=0; i<this.engine.missiles.length;i++)
        this.predictMissile(this.engine.missiles[i]);
    for (let i=0; i<this.engine.shells.length;i++)
        this.predictShell(this.engine.shells[i]);
    for (let i=0; i<this.engine.asteroids.length;i++)
        this.predictAsteroid(this.engine.asteroids[i]);
    for (let i=0; i<this.engine.spheres.length;i++)
        this.predictSphere(this.engine.spheres[i]);
};

WebClient.prototype.translateServerDefault = function (local, server) {
    local.position.x = server[1];
    local.position.y = server[2];
    local.angle = server[3];
};

WebClient.prototype.translateServerMissile = function (local, server) {
    local.position.x = server[1];
    local.position.y = server[2];
    local.velocity.x = server[3];
    local.velocity.y = server[4];
    local.angle = server[5];
};


WebClient.prototype.translateServerSphere = function (sphere, server) {
    sphere.position.x = server[1];
    sphere.position.y = server[2];
    sphere.velocity.x = server[3];
    sphere.velocity.y = server[4];
    sphere.target = server[5];
};

WebClient.prototype.processServerThingUpdate = function (thing, things, translateFn, cache) {
    var match = -1;
    for (let j=0; j<things.length; j++) {
        if (things[j].id === thing[0]) {
            match = j;
            break;
        }
    }
    if (match >= 0) {
        translateFn(things[match], thing);
        things[match].active = true;
        cache.add(things[match]);
        return things[match];
    } else {
        var o = {position: {x:0, y:0},
                 velocity: {x:0, y:0},
                 angle: 0,
                 id: thing[0],
                 active: true};
        translateFn(o, thing);
        things.push(o);
        cache.add(o);
        return o;
    }
}

WebClient.prototype.processServerShellUpdate = function (shell) {
    this.processServerThingUpdate(shell, this.engine.shells,
                                  this.translateServerDefault.bind(this),
                                  this.three.shellCache);
};

WebClient.prototype.processServerMissileUpdate = function (missile) {
    this.processServerThingUpdate(missile, this.engine.missiles,
                                  this.translateServerMissile.bind(this),
                                  this.three.missileCache);
};

WebClient.prototype.processServerSphereUpdate = function (sphere) {
    var o = this.processServerThingUpdate(sphere, this.engine.spheres,
                                          this.translateServerSphere.bind(this),
                                          this.three.sphereCache);
};


WebClient.prototype.processServerUpdates = function () {
    if (this.network.serverUpdates.length === 0) {
        for (let i=0; i<this.engine.players.length; i++) {
            if (this.engine.players[i].id !== this.id)
                this.predictPlayer(this.engine.players[i]);
        }
        this.predictiveStep();
    } else {
        // Scan all updates for messages
        for (let i=0; i<this.network.serverUpdates.length; i++) {
            var msg = this.network.serverUpdates[i].msg;
            for (let i=0; i<msg.length;i++) {
                this.engine.messages.push({player: this.engine.getPlayer(msg[i][0]), msg: msg[i][1], tick: this.engine.ticks});
                g_sounds[msg[i][1]].play();
            }
        }
        // We only need the last update
        var last = this.network.serverUpdates[this.network.serverUpdates.length-1];
        this.engine.points = last.points;
        $('#points').html(this.engine.points);
        var players = last.p;
        for (let i=0;i<players.length;i++) {
            // console.log('update', k, players[k], this.engine.players[k])
            if (this.engine.players[i]) {
                this.engine.players[i].alive = players[i][0],
                this.engine.players[i].position.x = players[i][1];
                this.engine.players[i].position.y = players[i][2];
                this.engine.players[i].angle = players[i][3];
                this.engine.players[i].velocity.x = players[i][4];
                this.engine.players[i].velocity.y = players[i][5];
                this.engine.players[i].turnFlag = players[i][6];
            }
        }
        var fortresses = last.f;
        for (let i=0; i<this.engine.fortresses.length;i++) {
            this.engine.fortresses[i].alive = fortresses[i][0];
            this.engine.fortresses[i].position.x = fortresses[i][1];
            this.engine.fortresses[i].position.y = fortresses[i][2];
            this.engine.fortresses[i].angle = fortresses[i][3];
        }
        var shells = last.s;
        this.three.shellCache.clear();
        for (let i=this.engine.shells.length-1; i>=0; i--) {
            this.engine.shells[i].active = false;
        }
        for (let i=0; i<shells.length; i++) {
            this.processServerShellUpdate(shells[i]);
        }
        for (let i=this.engine.shells.length-1; i>=0; i--) {
            if (!this.engine.shells[i].active) this.engine.shells.splice(i,1);
        }

        var missiles = last.m;
        this.three.missileCache.clear();
        for (let i=this.engine.missiles.length-1; i>=0; i--) {
            this.engine.missiles[i].active = false;
        }
        for (let i=0; i<missiles.length; i++) {
            this.processServerMissileUpdate(missiles[i]);
        }
        for (let i=this.engine.missiles.length-1; i>=0; i--) {
            if (!this.engine.missiles[i].active) this.engine.missiles.splice(i,1);
        }

        var spheres = last.spheres;
        this.three.sphereCache.clear();
        for (let i=this.engine.spheres.length-1; i>=0; i--) {
            this.engine.spheres[i].active = false;
        }
        for (let i=0; i<spheres.length; i++) {
            this.processServerSphereUpdate(spheres[i]);
        }
        for (let i=this.engine.spheres.length-1; i>=0; i--) {
            if (!this.engine.spheres[i].active) this.engine.spheres.splice(i,1);
        }

        var asteroids = last.a;
        for (let i=0; i<this.engine.asteroids.length; i++) {
            this.engine.asteroids[i].position.x = asteroids[i][0];
            this.engine.asteroids[i].position.y = asteroids[i][1];
            this.engine.asteroids[i].angle = asteroids[i][2];
        }

        var lastMovementRequest = last.lmr;
        for (let i=0; i<this.movementRequests.length; i++) {
            if (lastMovementRequest === this.movementRequests[i][0]) {
                this.movementRequests.splice(0,i+1);
                break;
            }
        }
        this.replayMovementRequests();
        // This should reduce rubber-banding when latency is high
        for (let i=0; i<this.movementRequests.length; i++)
            this.predictiveStep();

        this.network.serverUpdates.length = 0;
    }
};

WebClient.prototype.drawExplosion = function(ctx, x, y) {
    var ofs = 0;
    var radius;
    for (radius=15; radius<70; radius+=8) {
        var angle;
        ofs += 3;
        if (radius < 60) { ctx.strokeStyle = '#FFFF00'; }
        else { ctx.strokeStyle = '#FF0000'; }
        for (angle=0; angle<360; angle += 30) {
            ctx.beginPath();
            ctx.arc(x, y, radius, deg2rad(angle+ofs),deg2rad(angle+ofs+10),false);
            ctx.stroke();
        }
    }
    ctx.strokeStyle = '#FFFF00';
    ctx.beginPath();
    ctx.arc(x, y, 7, 0, Math.PI*2, false);
    ctx.stroke();
};

WebClient.prototype.roundRect = function (ctx, x, y, width, height, radius) {
    radius = radius || 5;
    if (typeof radius === 'number') {
        radius = {tl: radius, tr: radius, br: radius, bl: radius};
    } else {
        var defaultRadius = {tl: 0, tr: 0, br: 0, bl: 0};
        for (var side in defaultRadius) {
            radius[side] = radius[side] || defaultRadius[side];
        }
    }
    ctx.beginPath();
    ctx.moveTo(x + radius.tl, y);
    ctx.lineTo(x + width - radius.tr, y);
    ctx.quadraticCurveTo(x + width, y, x + width, y + radius.tr);
    ctx.lineTo(x + width, y + height - radius.br);
    ctx.quadraticCurveTo(x + width, y + height, x + width - radius.br, y + height);
    ctx.lineTo(x + radius.bl, y + height);
    ctx.quadraticCurveTo(x, y + height, x, y + height - radius.bl);
    ctx.lineTo(x, y + radius.tl);
    ctx.quadraticCurveTo(x, y, x + radius.tl, y);
    ctx.closePath();
};

WebClient.prototype.HUDDirection2 = function (from, to) {
    var a = stdAngle(angleTo(from, to));
    return {x:this.canvas2d.width/2+Math.cos(deg2rad(a)) * 245,
            y:this.canvas2d.height/2+Math.sin(deg2rad(a)) * 245};
};

WebClient.prototype.HUDDirection = function (from, to) {
    var a = stdAngle(angleTo(from, to));
    var p1 = {x:this.canvas2d.width/2, y:this.canvas2d.height/2};
    var p2 = {x:p1.x + Math.cos(deg2rad(a)),
              y:p1.y + Math.sin(deg2rad(a))};
    var p3, p4;

    var corners = [rad2deg(Math.atan2(this.canvas2d.height/2,this.canvas2d.width/2)),
                   rad2deg(Math.atan2(this.canvas2d.height/2,-this.canvas2d.width/2)),
                   stdAngle(rad2deg(Math.atan2(-this.canvas2d.height/2,-this.canvas2d.width/2))),
                   stdAngle(rad2deg(Math.atan2(-this.canvas2d.height/2,this.canvas2d.width/2)))];

    // console.log(corners);

    // start_a = 45;

    var x,y;
    // console.log(a);
    if (a <= corners[0]) {
        p3 = {x:this.canvas2d.width-5, y: 5};
        p4 = {x:this.canvas2d.width-5, y: this.canvas2d.height-5};
    } else if (a <= corners[1]) {
        p3 = {x:5, y: this.canvas2d.height-5};
        p4 = {x:this.canvas2d.width-5, y: this.canvas2d.height-5};
    } else if (a <= corners[2]) {
        p3 = {x:5, y: 5};
        p4 = {x:5, y: this.canvas2d.height-5};
    } else if (a <= corners[3]) {
        p3 = {x:5, y:5};
        p4 = {x:this.canvas2d.width-5, y:5};
    } else {
        p3 = {x:this.canvas2d.width-5, y:5};
        p4 = {x:this.canvas2d.width-5, y:this.canvas2d.height-5};
    }
    return lines_intersection_point(p1,p2,p3,p4)[1];
}

WebClient.prototype.drawEgocentric = function () {
    // this.camera.position.x = this.engine.config.mapSize * this.engine.config.mapCellSize/2;
    // this.camera.position.y = this.engine.config.mapSize * this.engine.config.mapCellSize/2;
    // this.camera.position.z = 300;
    // this.camera.lookAt(this.engine.players[this.id].mesh.position);
    var p = this.engine.getPlayer(this.id);


    if (this.cameraMode === 'chase') {
        this.camera.position.x = p.position.x - Math.cos(p.angle * Math.PI/180)*200;
        this.camera.position.y = p.position.y - Math.sin(p.angle * Math.PI/180)*200;
        this.camera.position.z = 200;
        this.camera.lookAt(p.mesh.position);
    } else if (this.cameraMode === 'overhead') {
        this.camera.position.x = p.position.x;
        this.camera.position.y = p.position.y;
        this.camera.position.z = 500;
        this.camera.lookAt(p.mesh.position);
    } else if (this.cameraMode === 'stationary') {
        this.camera.position.x = this.engine.config.mapSize * this.engine.config.mapCellSize/2;
        this.camera.position.y = this.engine.config.mapSize * this.engine.config.mapCellSize/2;
        this.camera.position.z = 300;
        this.camera.lookAt(p.mesh.position);
    } else if (this.cameraMode === 'muzzle') {
        this.camera.position.x = p.position.x - Math.cos(p.angle * Math.PI/180)*0.5;
        this.camera.position.y = p.position.y - Math.sin(p.angle * Math.PI/180)*0.5;
        this.camera.position.z = 30;
        this.camera.lookAt(p.mesh.position);
        // this.camera.position.x += Math.cos(p.angle * Math.PI/180)*30;
        // this.camera.position.y += Math.cos(p.angle * Math.PI/180)*30;
        this.camera.position.z = 45;
    }

    this.renderer.render(this.scene, this.camera);
};

WebClient.prototype.drawGameState = function () {
    var p = this.engine.getPlayer(this.id);

    this.ctx.save();
    this.ctx.clearRect(0,0,this.canvas2d.width,this.canvas2d.height);
    this.ctx.translate(-p.position.x+this.canvas2d.width/2,
                       -p.position.y+this.canvas2d.height/2);
    this.ctx.strokeStyle = '#003300';
    var maxx = this.engine.config.mapSize * this.engine.config.mapCellSize;
    var maxy = this.engine.config.mapSize * this.engine.config.mapCellSize;
    for (let x=0; x<this.engine.config.mapSize; x++) {
        this.ctx.moveTo(x*this.engine.config.mapCellSize, 0);
        this.ctx.lineTo(x*this.engine.config.mapCellSize, maxy);
    }
    for (let y=0; y<this.engine.config.mapSize; y++) {
        this.ctx.moveTo(0, y*this.engine.config.mapCellSize);
        this.ctx.lineTo(maxx, y*this.engine.config.mapCellSize);
    }
    this.ctx.stroke();
    this.ctx.lineWidth = 4;
    this.ctx.strokeStyle = '#00AA00';
    this.ctx.strokeRect(0, 0, maxx, maxy);
    this.ctx.lineWidth = 1;
    for (let i=0; i<this.engine.fortresses.length; i++) {
        this.engine.hexagons[this.engine.fortresses[i].radius].draw(this.ctx,
                                                                    this.engine.fortresses[i].position.x,
                                                                    this.engine.fortresses[i].position.y,
                                                                    0,
                                                                    '#999999');
        if (this.engine.fortresses[i].alive) {
            // this.ctx.beginPath();
            // this.ctx.arc(this.engine.fortresses[i].position.x,
            //              this.engine.fortresses[i].position.y,
            //              30,
            //              deg2rad(this.engine.fortresses[i].angle)-Math.PI*5/8,
            //              deg2rad(this.engine.fortresses[i].angle)+Math.PI*5/8);
            // this.ctx.strokeStyle = '#FFFF00';
            // this.ctx.stroke();
            this.engine.hexagons[this.engine.fortresses[i].radius].draw(this.ctx,
                                                                        this.engine.fortresses[i].position.x,
                                                                        this.engine.fortresses[i].position.y,
                                                                        0,
                                                                        '#999999');
            this.engine.hexagons[this.engine.config.fortress.smallHex].fill(this.ctx,
                                                                            this.engine.fortresses[i].position.x,
                                                                            this.engine.fortresses[i].position.y,
                                                                            this.engine.fortresses[i].angle,
                                                                            '#000000');
            this.ctx.strokeStyle = '#003300';
            this.ctx.stroke();
            this.ctx.lineWidth = 1.5;
            this.engine.hexagons[this.engine.config.fortress.smallHex].drawPartial(this.ctx,
                                                                                   this.engine.fortresses[i].position.x,
                                                                                   this.engine.fortresses[i].position.y,
                                                                                   this.engine.fortresses[i].angle,
                                                                                   '#00FF00');
            this.ctx.lineWidth = 1;

            fortressWireframe.draw(this.ctx,
                                   this.engine.fortresses[i].position.x,
                                   this.engine.fortresses[i].position.y,
                                   this.engine.fortresses[i].angle);

            // this.ctx.beginPath();
            // this.ctx.arc(this.engine.fortresses[i].position.x,
            //              this.engine.fortresses[i].position.y,
            //              this.engine.fortresses[i].radius,
            //              0, Math.PI*2);
            // this.ctx.strokeStyle = '#999999';
            // this.ctx.stroke();
        }

    }
    for (let i=0; i<this.engine.players.length; i++) {
        if (this.engine.players[i].alive)
            shipWireframe.draw (this.ctx,
                                this.engine.players[i].position.x,
                                this.engine.players[i].position.y,
                                this.engine.players[i].angle,
                                this.engine.players[i].color);
        else
            this.drawExplosion(this.ctx,
                               this.engine.players[i].position.x,
                               this.engine.players[i].position.y);
    }

    // Projectiles
    for (let i=0; i<this.engine.missiles.length;i++) {
        missileWireframe.draw(this.ctx,
                              this.engine.missiles[i].position.x,
                              this.engine.missiles[i].position.y,
                              this.engine.missiles[i].angle);
    }
    for (let i=0; i<this.engine.shells.length;i++) {
        shellWireframe.draw(this.ctx,
                            this.engine.shells[i].position.x,
                            this.engine.shells[i].position.y,
                            this.engine.shells[i].angle);
    }
    // asteroids
    this.ctx.strokeStyle = '#FF3333';
    this.ctx.fillStyle = '#3333FF';
    for (let i=0; i<this.engine.asteroids.length;i++) {
        this.ctx.save();
        this.ctx.translate(this.engine.asteroids[i].position.x,
                           this.engine.asteroids[i].position.y);
        this.ctx.rotate(deg2rad(this.engine.asteroids[i].angle));
        for (let j=0; j<this.engine.asteroids[i].bubbles.length; j++) {
            this.ctx.beginPath();
            this.ctx.arc(this.engine.asteroids[i].bubbles[j].x,
                         this.engine.asteroids[i].bubbles[j].y,
                         this.engine.asteroids[i].bubbles[j].r,
                         0, Math.PI*2);
            this.ctx.fill();
            // this.ctx.stroke();
        }
        this.ctx.restore();
    }

    this.ctx.restore();

    // Way points
    var self = this.engine.getPlayer(this.id);
    for (let i=0; i<this.engine.players.length; i++) {
        if (this.engine.players[i] === self) continue;
        if (Math.abs(self.position.x - this.engine.players[i].position.x) < this.canvas2d.width/2 &&
            Math.abs(self.position.y - this.engine.players[i].position.y) < this.canvas2d.height/2)
            continue;
        var pos = this.HUDDirection(self.position, this.engine.players[i].position);
        this.ctx.fillStyle = this.engine.players[i].color;
        this.ctx.beginPath();
        this.ctx.arc(pos.x, pos.y, 5, 0, Math.PI*2);
        this.ctx.fill();

        // var pos2 = this.HUDDirection2(this.engine.players[this.id].position, this.engine.players[k].position);
        // this.ctx.fillStyle = this.engine.players[k].color;
        // this.ctx.beginPath();
        // this.ctx.arc(pos2.x, pos2.y, 5, 0, Math.PI*2);
        // this.ctx.fill();
        // this.ctx.beginPath();
        // this.ctx.arc(this.canvas2d.width/2, this.canvas2d.height/2, 245, 0, Math.PI*2);
        // this.ctx.strokeStyle = "#EEEEEE";
        // this.ctx.stroke();
    }

    // Messages
    this.ctx.save();
    this.ctx.font = '14px sans-serif';
    var y = 10;
    var fadeThreshold = 30;
    var height = 40;
    for (let i=0; i<this.engine.messages.length; i++) {
        // console.log('msg', this.engine.messages[i]);
        var txt = this.messageText[this.engine.messages[i].msg];
        var w = this.ctx.measureText(txt).width + 20;
        var diff = (this.engine.messages[i].tick + this.engine.config.message.duration) - this.engine.ticks;
        this.roundRect(this.ctx, 10, y, Math.max(100, w), 35);
        this.ctx.strokeStyle = this.engine.messages[i].player.color;
        this.ctx.fillStyle = '#000000';

        if (diff < fadeThreshold) this.ctx.globalAlpha = diff/fadeThreshold * 0.7;
        else this.ctx.globalAlpha = 1;

        this.ctx.fill();
        this.ctx.stroke();
        this.ctx.fillStyle = this.engine.messages[i].player.color;
        // this.ctx.fillStyle = '#000000';
        this.ctx.fillText(txt, 20, y + 5+16);
        this.ctx.globalAlpha = 1;

        if (diff < fadeThreshold) y -= height - height * (diff*1.0)/fadeThreshold;
        y += height;
    }
    this.ctx.restore();



};

WebClient.prototype.updateMessages = function () {
    for (let i=this.engine.messages.length-1; i>=0; i--) {
        if (this.engine.messages[i].tick + this.engine.config.message.duration <= this.engine.ticks ) {
            this.engine.messages.splice(i, 1);
        }
    }
};

WebClient.prototype.saveState = function () {
    exp.log.push(this.engine.dumpState());
    // This is zero'd out in step_one_tick but the client doesn't call
    // that so we need to do this manually.
    this.engine.events.length = 0;
    if (exp.log.length >= exp.logSyncLength) {
        console.log('syncing game state');
        exp.com.synchronizeLog(exp.log);
    }
};

WebClient.prototype.requestUpdate = function () {
    this.updateid = window.requestAnimationFrame( this.update.bind(this), this.canvas );
}

WebClient.prototype.update = function (t) {
    this.dt = this.lastUpdateTime ? (t - this.lastUpdateTime) : 1000/60.0;
    this.lastUpdateTime = t;
    // If the frame rate drops below 60fps, we need to catch up by
    // doing doing multiple game ticks.
    var ms = this.leftOverDT + this.dt;
    var ticks = Math.floor(ms / (1000/60.0));
    this.leftOverDT = ms - ticks * 1000/60.0;
    // console.log(this.lastUpdateTime, this.dt, ms, ticks, this.leftOverDT);
    for (let i=0; i<ticks; i++) {
        this.engine.ticks += 1;
        this.processKbdInput();
        this.processServerUpdates();
        this.saveState();
    }
    this.updateScene();

    if (this.cameraMode === '2d')
        this.drawGameState();
    else
        this.drawEgocentric();
    this.updateMessages();


    this.requestUpdate();
};
