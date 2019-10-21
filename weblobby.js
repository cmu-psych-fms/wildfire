function WebLobby () {

}

WebLobby.prototype = {};

WebLobby.prototype.updateHTML = function () {
    var html = '';

    for (let i=0; i<this.clients.length; i++) {
        html += '<div class="lobby_'+this.clients[i].mode+'" id="'+this.clients[i].id+'">';
        html += '<div style="background-color: #000000; color: #ffffff; margin-bottom: 5px">'+this.clients[i].mode+'</div>';
        if (this.clients[i].ready)
            html += 'Ready';
        else
            html += 'Client '+(i+1);
        // I don't know why but without the invisible buttons, the divs don't line up!!
        var hasId = this.clients[i].id === this.id;
        var visible = hasId && !this.clients[i].ready;
        html += '<div id="client_buttons" style="visibility:'+ (visible ? 'visible':'hidden')+'">';
        html += '<button id="'+(hasId?"setPlayerMode":"")+'">Player</button><br>';
        html += '<button id="'+(hasId?"setObserverMode":"")+'">Observer</button>';
        html += '<button id="'+(hasId?"ready":"")+'">Ready</button>';
        html += '</div>';

        html += '</div>';
    }
    console.log('update', this.clients);
    document.getElementById('lobby').innerHTML = html;

    document.getElementById('setPlayerMode').addEventListener('click', this.setPlayerMode.bind(this));
    document.getElementById('setObserverMode').addEventListener('click', this.setObserverMode.bind(this));
    document.getElementById('ready').addEventListener('click', this.setReady.bind(this));

};

WebLobby.prototype.setPlayerMode = function () {
    this.socket.emit('mode', 'player');
};

WebLobby.prototype.setObserverMode = function () {
    this.socket.emit('mode', 'observer');
};

WebLobby.prototype.setReady = function () {
    this.socket.emit('ready', true);

    // document.getElementById('client_buttons').style.visibility = 'hidden';
};

WebLobby.prototype.listen = function () {
    this.socket.on('connect', this.onConnect.bind(this));
    this.socket.on('disconnect', this.onDisconnect.bind(this));
    this.socket.once('roster', this.onJoin.bind(this));
    this.socket.on('start', this.onStartGame.bind(this));
};

WebLobby.prototype.begin = function () {
    this.clients = [];
    this.socket = io.connect({reconnection: false});

    this.listen();
};

WebLobby.prototype.onConnect = function() {
    console.log('connected');
};

WebLobby.prototype.onDisconnect = function() {
    console.log('disconnected');
};

WebLobby.prototype.onJoin = function(data) {
    this.id = data.join;
    this.clients = data.clients;
    this.socket.on('roster', this.onRosterUpdate.bind(this));
    this.updateHTML();
    console.log('this client is', this.id);
};

WebLobby.prototype.onRosterUpdate = function(data) {
    console.log('roster update');
    this.clients = data.clients;

    this.updateHTML();
};

WebLobby.prototype.onStartGame = function(data) {
    console.log('start the game');
    this.socket.off('connect');
    this.socket.off('disconnect');

    document.getElementById('gamearea').style.display = 'block';
    document.getElementById('menu').style.display = 'block';
    document.getElementById('lobbyarea').style.display = 'none';

    var wc = new WebClient();
    g_game = wc;
    wc.begin(this.socket, data);

    wc.on('end', this.backToLobby.bind(this));
};

WebLobby.prototype.backToLobby = function() {
    console.log('back to lobby');
    this.clients = [];

    document.getElementById('instructions_area').style.display = 'none';
    document.getElementById('gamearea').style.display = 'none';
    document.getElementById('menu').style.display = 'none';
    document.getElementById('lobbyarea').style.display = 'block';

    this.socket.on('connect', this.onConnect.bind(this));
    this.socket.on('disconnect', this.onDisconnect.bind(this));
};
