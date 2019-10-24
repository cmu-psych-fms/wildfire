import argparse
import random
import socketio

class Model(object):
    def __init__(self, num_players):
        self.num_players = num_players
        self.debug = 1
        self.state = 'disconnected'
        self.sio = socketio.Client()
        # There are the events we must listen for from the server.
        self.sio.on('connect', self.on_connect)
        # The server sends this event when a client first connects
        self.sio.on('welcome', self.on_welcome)
        # The server sends this event in the lobby
        self.sio.on('roster', self.on_roster)
        # The server sends this event when the server is ready for the
        # client to start playing.
        self.sio.on('start', self.on_start)
        # The server sends this event every 100ms. It contains a
        # everything the client needs to create the current game
        # state.
        self.sio.on('update', self.on_server_update)
        # The server sends this event when another player leaves
        self.sio.on('part', self.on_player_part)
        # Players and observers can restart the game from the
        # beginning. This event is sent in such a case.
        self.sio.on('end', self.on_game_end)
        # If the connection to the server closes
        self.sio.on('disconnect', self.on_disconnect)

    def debug_recv(self, event, data):
        if self.debug >= 2:
            print('<<<', event, data)

    def debug_send(self, event, data):
        if self.debug >= 2:
            print('>>>', event, data)

    def announce(self, msg, *args):
        if self.debug >= 1:
            print('---', msg, *args)

    def emit(self, event, data):
        self.debug_send(event, data)
        self.sio.emit(event, data)

    def connect_to_server(self, url):
        self.sio.connect(url)

    def on_connect(self):
        self.debug_recv('connected', None)
        self.announce('connected')

    def on_welcome(self, data):
        self.debug_recv('welcome', data)
        self.announce('welcome', data['id'])
        self.id = data['id']
        self.state = 'lobby'

    def update_player(self, player_id, data):
        self.players[player_id]['alive'] = data[0]
        self.players[player_id]['x'] = data[1]
        self.players[player_id]['y'] = data[2]
        self.players[player_id]['angle'] = data[3]
        self.players[player_id]['speed'] = data[4]
        self.players[player_id]['turnFlag'] = data[5]
        self.players[player_id]['water'] = data[6]

    def on_start(self, data):
        self.debug_recv('start', data)
        self.announce('start')
        # The data contains a snapshot of the game state: the map,
        # players, fires, retardant, and waypoints.
        self.id = data['id']
        self.server_game_tick = None
        self.map = data['map']
        self.players = {}
        self.last_confirmed_movement_sequence = None
        self.movement_request_sequence = 0
        for player_id in data['players']:
            self.players[player_id] = {}
            self.update_player(player_id, data['players'][player_id])
        # Start the client game loop
        self.state = 'game'
        self.game_thread = self.sio.start_background_task(self.client_game_loop)

    # Sample server update
    # {'t': 48,
    #  'p': {'e778c090-d0fb-11e9-82c9-1f5dc901c465': [1, 4000, 4000, 100, 0, 0, 50]},
    #  'm': [[90609, 2], [68568, 2], [81422, 2]],
    #  'f': [[2, {'x': 209, 'y': 226, 'level': 1}], [1, {'x': 210, 'y': 226, 'level': 1}], [1, {'x': 209, 'y': 227, 'level': 1}], [1, {'x': 209, 'y': 225, 'level': 1}], [2, {'x': 168, 'y': 171, 'level': 1}], [1, {'x': 168, 'y': 172, 'level': 1}], [1, {'x': 167, 'y': 171, 'level': 1}], [1, {'x': 168, 'y': 170, 'level': 1}], [2, {'x': 222, 'y': 203, 'level': 1}], [1, {'x': 223, 'y': 203, 'level': 1}], [1, {'x': 222, 'y': 204, 'level': 1}]],
    #  'r': [], 'wp': [], 'lk': {'seq': None, 'tick': None}}

    def on_server_update(self, data):
        # The server updates the game state every 15ms but the server
        # updates are only sent once every 100ms. This is done to
        # reduce the amount of data sent to the clients. Depending on
        # the pace of change and the game's features, more frequent
        # updates may be necessary. For the current version of
        # wildfire, 100ms seems to be fine.
        #
        # To create the illusion of smooth flying for humans, the web
        # client uses linear interpolation to smoothly move the other
        # airplanes' positions between updates.
        self.debug_recv('update', data)
        # Update the state of the players.
        for player_id in data['p']:
            self.update_player(player_id, data['p'][player_id])
        # Update the current game tick
        self.server_game_tick = data['t']
        # Changes to the map. Currently this is exclusively tiles
        # being replaced with ash after a fire has burned it.
        for m in data['m']:
            self.map[m[0]] = m[1]
        # Changes to the fire list.
        for f in data['f']:
            if f[0] == 1: # add fire
                self.map['fire'].append(f[1])
            elif f[0] == 2: # delete fire
                for i in range(len(self.map['fire'])):
                    if self.map['fire'][i]['x'] == f[1]['x'] and self.map['fire'][i]['y'] == f[1]['y']:
                        del self.map['fire'][i]
                        break
        # Changes to the retardant list.
        for r in data['r']:
            if r[0] == 0: # update retardant
                for i in range(len(self.map['retardant'])):
                    if self.map['retardant'][i]['x'] == r[1]['x'] and self.map['retardant'][i]['y'] == r[1]['y']:
                        self.map['retardant'][i] = r[1]
            if r[0] == 1: # add retardant
                self.map['retardant'].append(r[1])
            elif r[0] == 2: # delete retardant
                for i in range(len(self.map['retardant'])):
                    if self.map['retardant'][i]['x'] == r[1]['x'] and self.map['retardant'][i]['y'] == r[1]['y']:
                        del self.map['retardant'][i]
                        break
        # Changes to the way-point list
        for wp in data['wp']:
            if wp[0] == 1: # add a waypoint
                self.map['wayPoints'].append(wp[1])
        # The most recent movement sequence that the server has
        # received as of this server update. This is needed by the web
        # client to provide the illusion of lag-free movement for
        # human players. See the web client source code for how it is
        # used.
        self.last_confirmed_movement_sequence = data['lms']
        if 'vp' in data:
            # The amount of the map that is currently visible (aka
            # "viewport") has changed. This is an experimental feature
            # I was playing around with at one point. At the beginning
            # of the game, the visible part of the map is small, but
            # over time the map grows exposing more land that the
            # fires can burn.
            #
            # Every time the map grows, the server includes the new
            # visible portion of the map in the next update.
            self.map['viewPort'] = data['vp']

    def on_roster(self, data):
        self.debug_recv('roster', data)
        self.announce('roster')
        ready = False
        count = 0
        for c in data['clients']:
            if c['id'] == self.id:
                ready = c['ready']
            if c['mode'] == 'player':
                count += 1
        if not ready and count >= self.num_players:
            self.emit('ready', True)

    def on_player_part(self, data):
        self.debug_recv('part', data)
        self.announce('part', data['id'])
        # The player has disconnected from the game. Clean up their
        # data.
        del self.players[data['id']]

    def on_game_end(self, data):
        self.debug_recv('end', data)
        self.announce('game end')
        self.state = 'lobby'

    def on_disconnect(self):
        self.debug_recv('disconnect', None)
        self.announce('disconnected')
        self.state = 'disconnected'

    def send_movement_request(self, turn, thrust, dump, waypoints=[]):
        # Tell the server you'd like to move your water bomber. This
        # request happens 60 times a second--once per game tick. The
        # request includes:
        #
        # * the state of the turning (l=left, r=right, 0=fly straight),
        #
        # * the state of the "thrust" (f=speeding up, s=slowing down,
        # 0=unchanged)
        #
        # * the dump flag (1=dumping retardant, 0=not dumping)
        #
        # * A list of way points that you'd like to add (currenty
        # there is only 1=water). So if you want to place a water
        # waypoint, then waypoints should be [1].
        self.emit('movementRequest', [self.movement_request_sequence,
                                      turn,
                                      thrust,
                                      dump,
                                      waypoints])
        self.movement_request_sequence += 1

    def client_game_loop(self):
        # The client game loop sends movement requests at 60 frames
        # per second. It is these movement requests that make the
        # player's airplane move. Each movement request that the
        # server gets will step the airplane's movement by one
        # increment.
        #
        # In other words, without these movement requests the airplane
        # stands still. Obviously there is an easy game exploit: send
        # more movement requests than 60 per second and you can travel
        # around the map faster and put out more fires.
        #
        # If this was a real online game, the server would have
        # anti-cheating measures, such as ignoring the extra movement
        # requests.
        state = {'turn': 0,
                 'thrust': 0,
                 'dump': 0}
        waypoints = []
        ticks = 0
        while self.state == 'game':
            # Do something random every 3rd of a second
            if ticks % 20 == 0:
                k = random.choice(list(state.keys()))
                if k == 'turn':
                    state[k] = random.choice([0,'l', 'r'])
                elif k == 'thrust':
                    state[k] = random.choice([0,'f', 's'])
                elif k == 'dump':
                    state[k] = random.choice([0, 1])
            m.send_movement_request(state['turn'], state['thrust'], state['dump'], waypoints)
            self.sio.sleep(1/60.0)
            ticks += 1

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--server', metavar="URL", default="http://localhost:3000", help="The server to connect to")
    parser.add_argument('--players', type=int, default=2, help="Wait for this many players before starting a game.")
    parser.add_argument('--debug', help="Print verbose debug output", action='store_true')
    args = parser.parse_args()

    m = Model(args.players)
    if args.debug:
        m.debug = 2
    m.connect_to_server(args.server)
