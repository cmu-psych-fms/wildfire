# @license
# Copyright 2018,2019 Shawn Betts
# SPDX-License-Identifier: MIT

from __future__ import division, print_function
import sys
import argparse
import random
import json
import socket
import time
import math
import select

reference_time = [0]

def get_time():
    return int(round(time.time() * 1000)) - reference_time[0]

def distance(x1,y1,x2,y2):
    return math.sqrt((x1-x2)**2 + (y1-y2)**2)

def angle_to(sx, sy, ex, ey):
    a = math.atan2(ey-sy, ex-sx) * 180 / math.pi
    if a < 0:
        a += 360
    return a

def angle_diff(angle1, angle2):
    # It finally works--don't touch it!
    if angle1 < 0:
        angle1 += 360
    if angle1 > 360:
        angle1 %= 360
    if angle2 < 0:
        angle2 += 360
    if angle2 > 360:
        angle2 %= 360

    if angle1 < 90:
        if angle2 > angle1+180:
            return 360-angle2 + angle1
        else:
            return angle1 - angle2
    elif angle1 < 180:
        if angle2 > angle1+180:
            return 360-angle2 + angle1
        else:
            return angle1 - angle2
    elif angle1 < 270:
        if angle2 < angle1-180:
            return -(angle2 + 360-angle1)
        else:
            return angle1 - angle2
    else:
        if angle2 < angle1-180:
            return -(angle2 + 360-angle1)
        else:
            return angle1 - angle2

MAP_WATER = 4

CELLSIZE = 20

class Client(object):
    separator = b'\n'

    def __init__(self, agent):
        self.agent_class = agent
        self.debug = 1
        self.buffer = b""
        self.num_players = 2
        self.auto_abort = False
        self.auto_ready = False

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
        self.socket.sendall(json.dumps([event, data]).encode('utf-8') + self.separator)

    def read_from_socket(self):
        while True:
            idx = self.buffer.find(self.separator)
            if idx >= 0:
                raw = self.buffer[0:idx]
                self.debug_recv(raw, '')
                msg = json.loads(raw)
                self.buffer = self.buffer[idx+1:]
                return (msg[0],msg[1])
            try:
                bytes = self.socket.recv(1024)
                if len(bytes) == 0:
                    return ('disconnect', None)
                else:
                    self.buffer += bytes
            except socket.timeout:
                return (None,None)

    def read_from_socket_nonblocking(self):
        while True:
            idx = self.buffer.find(self.separator)
            if idx >= 0:
                raw = self.buffer[0:idx]
                self.debug_recv(raw, '')
                msg = json.loads(raw)
                self.buffer = self.buffer[idx+1:]
                return (msg[0],msg[1])

            inputs = [self.socket]
            (read, write, exception) = select.select(inputs, [], inputs, 0.0)

            if len(read) > 0 or len(exception) > 0:
                bytes = self.socket.recv(1024)
                if len(bytes) == 0:
                    return ('disconnect', None)
                else:
                    self.buffer += bytes
            else:
                return (None,None)


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
        # scan
        self.find_lakes()
        self.agent = self.agent_class()

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

    def on_player_part(self, data):
        self.debug_recv('part', data)
        self.announce('part', data['id']);
        # The player has disconnected from the game. Clean up their
        # data.
        del self.players[data['id']]
        if self.auto_abort:
            self.emit('abort', None)

    def on_disconnect(self):
        self.debug_recv('disconnect', None)
        self.announce('disconnected')

    def send_movement_request(self, turn, thrust, dump, waypoints=[]):
        # Tell the server you'd like to move your water bomber. This
        # request happens once per game tick, every 15ms. The
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

    def client_lobby_loop(self):
        # self.socket.setblocking(1)
        # self.socket.settimeout(None)
        while True:
            (event,data) = self.read_from_socket()
            if event == 'roster':
                self.announce('roster')
                ready = False
                ready_count = 0
                count = 0
                for c in data['clients']:
                    if c['id'] == self.id:
                        ready = c['ready']
                    if c['mode'] == 'player':
                        count += 1
                    if c['ready']:
                        ready_count += 1
                if not ready:
                    if self.auto_ready:
                        if ready_count > 0:
                            self.emit('ready', True)
                    else:
                        if count >= self.num_players:
                            self.emit('ready', True)
            elif event == 'start':
                self.on_start(data)
                return True
            elif event == 'disconnect':
                self.announce('disconnect')
                return False

    def lake_neighbors(self, i):
        neighbors = []
        for n in [i+1, i-1, i+self.map['width'], i-self.map['width']]:
            if n < 0:
                continue
            if n >= len(self.map['data']):
                continue
            if self.map['data'][n] != MAP_WATER:
                continue
            # Ignore horizontal rivers
            if ((n-1 < 0 or self.map['data'][n-1] == MAP_WATER) and
                (n+1 >= len(self.map['data']) or self.map['data'][n+1] == MAP_WATER) and
                (n-self.map['width'] < 0 or self.map['data'][n-self.map['width']] != MAP_WATER) and
                (n+self.map['width'] > len(self.map['data']) or self.map['data'][n+self.map['width']] != MAP_WATER)):
                # print('found horizontal river')
                continue
            # Ignore vertical rivers
            if ((n-1 < 0 or self.map['data'][n-1] != MAP_WATER) and
                (n+1 >= len(self.map['data']) or self.map['data'][n+1] != MAP_WATER) and
                (n-self.map['width'] < 0 or self.map['data'][n-self.map['width']] == MAP_WATER) and
                (n+self.map['width'] > len(self.map['data']) or self.map['data'][n+self.map['width']] == MAP_WATER)):
                # print('found vertical river')
                continue
            neighbors.append(n)
        return neighbors

    def find_lake_edges(self, seen, idx):
        frontier = [idx]
        boundingBox = {'left': idx%self.map['width'] * CELLSIZE,
                       'top': idx//self.map['width'] * CELLSIZE,
                       'right': idx%self.map['width'] * CELLSIZE,
                       'bottom': idx//self.map['width'] * CELLSIZE}

        while len(frontier) > 0:
            cur = frontier.pop()
            neighbors = self.lake_neighbors(cur)
            for n in neighbors:
                if n in seen:
                    continue
                frontier.append(n)
                seen[n] = True
                x = (n%self.map['width'])*CELLSIZE
                y = (n//self.map['width'])*CELLSIZE
                if x < boundingBox['left']:
                    boundingBox['left'] = x
                if x > boundingBox['right']:
                    boundingBox['right'] = x
                if y < boundingBox['top']:
                    boundingBox['top'] = y
                if y > boundingBox['bottom']:
                    boundingBox['bottom'] = y
        boundingBox['right'] += CELLSIZE
        boundingBox['bottom'] += CELLSIZE
        return boundingBox

    def is_surrounded_by_water(self, idx):
        return ((idx-1 >= 0 and self.map['data'][idx-1] == MAP_WATER) and
                (idx+1 < len(self.map['data']) and self.map['data'][idx+1] == MAP_WATER) and
                (idx-self.map['width'] >= 0 and self.map['data'][idx-self.map['width']] == MAP_WATER) and
                (idx+self.map['width'] < len(self.map['data']) and self.map['data'][idx+self.map['width']] == MAP_WATER))

    def find_lakes(self):
        seen = {}
        self.lakes = []
        for i,c in enumerate(self.map['data']):
            if c == MAP_WATER and i not in seen and self.is_surrounded_by_water(i):
                bb = self.find_lake_edges(seen, i)
                # # Ignore rivers
                # if bb['right'] - bb['left'] > CELLSIZE*3 and bb['bottom'] - bb['top'] > CELLSIZE*3:
                print('found lake', bb)
                self.lakes.append(bb)

    def get_nearest_lake(self, x, y):
        closest = self.lakes[0]
        d = distance(x, y, self.lakes[0]['left'], self.lakes[0]['top'])
        for l in self.lakes:
            tmp = distance(x, y, l['left'], l['top'])
            if tmp < d:
                closest = l
                d = tmp
        return closest

    def get_nearest_fire(self, x, y):
        if len(self.map['fire']) <= 0:
            return None
        closest = self.map['fire'][0]
        d = distance(x, y, self.map['fire'][0]['x']*CELLSIZE, self.map['fire'][0]['y']*CELLSIZE)
        for f in self.map['fire']:
            # Skip fires with nearby retardant
            skip = False
            for r in self.map['retardant']:
                if distance(f['x'], f['y'], r['x'],r['y']) < 2:
                    skip = True
                    break
            if skip:
                continue
            tmp = distance(x, y, f['x']*CELLSIZE, f['y']*CELLSIZE)
            if tmp < d:
                closest = f
                d = tmp
        return closest

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
        ticks = 0
        reference_time[0] = get_time()
        start_time = get_time()
        last_time = start_time
        spill_over = 0


        # self.socket.setblocking(0)
        # self.socket.settimeout(0.0)
        while True:
            done = False
            while not done:
                now = get_time()
                (event,data) = self.read_from_socket_nonblocking()
                if get_time() - now > 10:
                    print('what?', get_time() - now)
                if event == 'update':
                    self.on_server_update(data)
                elif event == 'part':
                    self.on_player_part(data)
                elif event == 'end':
                    self.announce('end')
                    return True
                elif event == 'disconnect':
                    self.announce('disconnect')
                    return False
                elif event == None:
                    done = True

            before_time = get_time()

            self.agent.step(self)
            ticks += 1

            # Maintain 15ms tick rate
            t = get_time()
            ticks_allowed = int((t - start_time) / 15)
            if ticks > ticks_allowed:
                target = get_time() + 15 * (ticks - ticks_allowed)
                # In theory this is more accurate and only slightly
                # more CPU intensive.
                while get_time() < target:
                    time.sleep(0.001)
            # if ticks % 200 == 0:
            #     print('actual ticks', ticks, 'vs expected', int((t - start_time) / 15))

    def connect_to_server(self, host, port):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((host,port))
        self.socket.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 1)
        self.announce('connected')

    def main_loop(self):
        # When we join
        (event,data) = self.read_from_socket()
        if event == 'welcome':
                self.id = data['id']
        else:
            raise Exception('expected a welcome event to get our id')
        while True:
            if not self.client_lobby_loop():
                break
            if not self.client_game_loop():
                break

class random_agent(object):
    def __init__(self):
        self.state = {'turn': 0,
                      'thrust': 0,
                      'dump': 0}
        self.waypoints = []

    def step(self, client):
        """Do something random every 3rd of a second."""
        if client.movement_request_sequence % 20 == 0:
            k = random.choice(list(self.state.keys()))
            if k == 'turn':
                self.state[k] = random.choice([0,'l', 'r'])
            elif k == 'thrust':
                self.state[k] = random.choice([0,'f', 's'])
            elif k == 'dump':
                self.state[k] = random.choice([0, 1])
        client.send_movement_request(self.state['turn'], self.state['thrust'], self.state['dump'], self.waypoints)

class smrt_agent(object):
    def __init__(self):
        self.debug = False
        self.set_state('fly-to-fire')

    def set_state(self, state):
        self.play_state = state
        if self.debug:
            print(self.play_state)

    def step(self, client):
        state = {'turn': 0,
                 'thrust': 0,
                 'dump': 0}
        waypoints = []

        if self.play_state == 'fly-to-fire':
            before = get_time()
            closest_fire = client.get_nearest_fire(client.players[client.id]['x'], client.players[client.id]['y'])
            if closest_fire:
                a = angle_to(client.players[client.id]['x'], client.players[client.id]['y'],
                             closest_fire['x']*CELLSIZE, closest_fire['y']*CELLSIZE)
                d = angle_diff(client.players[client.id]['angle'], a)
                if d > 3:
                    state['turn'] = 'l'
                elif d < -3:
                    state['turn'] = 'r'
                else:
                    state['turn'] = 0
                dist = distance(client.players[client.id]['x'], client.players[client.id]['y'],
                                closest_fire['x']*CELLSIZE+CELLSIZE/2, closest_fire['y']*CELLSIZE+CELLSIZE/2)
                if dist < 100:
                    state['thrust'] = 's'
                elif client.players[client.id]['speed'] < 5:
                    state['thrust'] = 'f'
                if dist < 30:
                    self.dump_amt = 2
                    self.retardants_dumped = []
                    self.set_state('dump')
            else:
                self.set_state('hover')
        elif self.play_state == 'dump':
            has = False
            for r in self.retardants_dumped:
                if (r['x'] == client.players[client.id]['x'] // CELLSIZE and
                    r['y']  == client.players[client.id]['y'] // CELLSIZE):
                    has = True
                    break
            if not has:
                state['dump'] = 1
                r = {'x': client.players[client.id]['x'] // CELLSIZE,
                     'y': client.players[client.id]['y'] // CELLSIZE}
                self.retardants_dumped.append(r)
            if client.players[client.id]['water'] > 0:
                if len(self.retardants_dumped) >= self.dump_amt:
                    self.set_state('fly-to-fire')
            else:
                self.set_state('fly-to-lake')
                self.got_inside = False
        elif self.play_state == 'fly-to-lake':
            closest_lake = client.get_nearest_lake(client.players[client.id]['x'], client.players[client.id]['y'])

            midx = closest_lake['left'] + (closest_lake['right'] - closest_lake['left'])/2
            midy = closest_lake['top'] + (closest_lake['bottom'] - closest_lake['top'])/2
            a = angle_to(client.players[client.id]['x'], client.players[client.id]['y'],
                         midx, midy)
            d = angle_diff(client.players[client.id]['angle'], a)
            if d > 3:
                state['turn'] = 'l'
            elif d < -3:
                state['turn'] = 'r'
            else:
                state['turn'] = 0

            if distance(client.players[client.id]['x'], client.players[client.id]['y'], midx, midy) < 150:
                if client.players[client.id]['speed'] > 1:
                    state['thrust'] = 's'
            else:
                if client.players[client.id]['speed'] < 5:
                    state['thrust'] = 'f'


            if (client.players[client.id]['x'] >= closest_lake['left'] and
                client.players[client.id]['x'] <= closest_lake['right'] and
                client.players[client.id]['y'] >= closest_lake['top'] and
                client.players[client.id]['y'] <= closest_lake['bottom']):
                self.got_inside = True
                state['dump'] = 1
            else:
                if self.debug and self.got_inside:
                    print('left but not filled', a, client.players[client.id]['angle'], d, midx, midy, client.players[client.id]['x'], client.players[client.id]['y'])

            if client.players[client.id]['water'] >= 50:
                self.set_state('fly-to-fire')
        elif self.play_state == 'hover':
            # A slow counter-clockwise circle
            state['thrust'] = 's'
            state['turn'] = 'l'
        else:
            raise Exception('unknown play state', self.play_state)

        client.send_movement_request(state['turn'], state['thrust'], state['dump'], waypoints)

        # Simulate the plane's flight for better precision. These
        # values will be overwritten with official values on the next
        # server update.
        if state['turn'] == 'l':
            client.players[client.id]['angle'] -= 3
        elif state['turn'] == 'r':
            client.players[client.id]['angle'] += 3
        if state['thrust'] == 'f':
            client.players[client.id]['speed'] += 0.04
        elif state['thrust'] == 's':
            client.players[client.id]['speed'] -= 0.04
        client.players[client.id]['x'] += client.players[client.id]['speed'] * math.cos(client.players[client.id]['angle'] * math.pi / 180);
        client.players[client.id]['y'] += client.players[client.id]['speed'] * math.sin(client.players[client.id]['angle'] * math.pi / 180);



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--host', metavar="URL", default="localhost", help="The server to connect to")
    parser.add_argument('--port', metavar="URL", type=int, default=3001, help="The port to connect to")
    parser.add_argument('--players', type=int, default=2, help="Wait for this many players before starting a game.")
    parser.add_argument('--auto-abort', action='store_true', help="Abort the game as soon as anyone else parts.")
    parser.add_argument('--auto-ready', action='store_true', help="Send the ready signal when anyone else is ready.")
    parser.add_argument('--agent', default='smrt', metavar="AGENT", help="Choose which agent to use (options: smrt, random)")
    parser.add_argument('--debug', help="Print verbose debug output", action='store_true')
    args = parser.parse_args()

    agents = {'smrt': smrt_agent,
              'random': random_agent}
    if args.agent not in agents:
        raise Exception('unknown agent', args.agent)

    c = Client(agents[args.agent])
    if args.debug:
        c.debug = 2
    c.auto_abort = args.auto_abort
    c.auto_ready = args.auto_ready
    c.num_players =args.players
    c.connect_to_server(args.host, args.port)
    c.main_loop()
