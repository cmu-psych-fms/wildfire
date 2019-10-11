### What Is This?

This is a cooperative multiplayer game. The goal is to put out all the
fires by dumping fire retardant on them.

Players control a waterbomber flying over a 2D map.

Use Up or W to increase flying speed

Use Down or S to decrease flying speed

Use Left or A to rotate counter-clockwise

Use Right or D to rotate clockwise

Use SPACE to dump fire retardant and pick up water

Use 1 to drop a water beacon at the current location.

Gamepads are also supported.

The blue bar at the top of the game area indicates how much water your
plane is carrying. When it runs out you must refill at the nearest
body of water. To refill, reduce your flight speed all the way down
and hold down SPACE while flying over the water.

To help you return to a body of water, press 1 when flying over the
water to drop a beacon. A blue dot will appear at the edge of the game
area indicating the direction of the body of water from your current
position.

Brown rectangles at the edge of the game area indicate the direction to other players.

Click Reset Game to reset the map for all players.

### Number of players

Any number of of players can connect and play together.

### How To Run This Program

1. install node.js

2. install the required libraries:

    `npm install`

3. Run the server:

    `node nodeserver.js`

3. The server listens on port 3000. Connect a browser (I recommend
Chrome or Firefox) to the server. For instance, to connect on the same
machine as the server, open the URL:

    `http://localhost:3000`

4. Open a browser on a second machine for the second player.

### Observer vs Player Mode

Web browser clients can connect as a player or an observer. A player
controls a ship and participates in the game. An observer only watched
the game.

Observer clients start in "ghost" mode where they can use the `arrow
keys` to move their viewable area of the map. Press the keys `1`
through `9` to switch to "follow" mode and follow a particular
player's airplane. Press `0` to return to "ghost" mode.

There is currently no way to zoom in/out or view the entire map as an
observer.

### Disconnect

Players and observers can press `Escape` to disconnect from the server
and stop the game updates.

### Logging

The server can record record game data to a log file. For now, the
update events sent from server are all that is recorded to the log
file. This means not every game tick is recorded. Future updates
will improve this.

The Log is a JSON formatted object. The top-level object contains
information about the session and a `"body"` property. `"body"` is a
list whose entries are either `lobby` or `game` screens. The `lobby`
screens record `"roster"` events sent in the lobby. The `game` screens
record `"update"` events sent during the game.

Each screen is an object with its own `"body"` property. `"body"` is a
list containing the events sent from the server while that screen was
active.

Use the `--log` command line option to enable logging. You can supply
an ID using the `--id` option that uniquely identifies this
session. For example:

    node nodeserver.js --log --id testmodel1

Log files are stored in the `logs` folder in the current
directory. The server will not overwrite log files. If the log file
exists, the server will choose an alternate, available filename.



### Model Interface

For browser-based clients, all communication is done using Socket.io
to exchange JSON strings between the server and clients.

I have written a demo python model player in `model_client.py` that
provides a basic example of how a model could connect to the server
and play the game using the socket.io interface. To try it you will
need the `python-socketio` python package and python 3:

    $ python3 model_client.py

If you'd like to see all data being sent and received from the server,
run it like this:

    $ python3 model_client.py --debug

For clients not running in the browser there is a second, simpler
protocol that runs on straight TCP/IP. The information exchanged
between client and server is exactly the same as the Socket.io
interface but without requiring Socket.io.

Again, all communication is based on sending JSON data between client
and server.

I have written a demo python model player in `model_client_tcp.py`
that uses the TCP/IP interface to connect to the server and play.

This TCP/IP interface should make it fairly easy to write an ACT-R
module in LISP that allows ACT-R models to connect to the server.

### Client/Server Communication

The following documents the communication protocol. The clients and
server communicate via "events" that are emitted using the `socket.io`
library. Each event has a name and data.

When a client first connects it will receive a `welcome` event.

###### "welcome" { "id": < client id > }

This tells the client their unique client ID.

#### Lobby

When clients connect they start in the lobby. This is where players
gather before starting a game.

##### Lobby Messages From the Server to the Client

###### "roster" { clients: [ ... ], join: < client id >, part: < client id > }

This event is sent to all clients when a player joins, parts, or
changes their mode or ready state. It describes the state of each
client in the lobby.

Each element of the `"clients"` list is an object with the following fields:

    { id: <client id>, mode: <string>, ready: <boolean> }

The client's mode can be `"observer"` or `"player"`. `"ready"` is
`true` when the client is ready to start the game and `false` if
not. When all clients are ready, the game starts.

##### Lobby Messages From client to the Server

###### "ready" < boolean >

Set the client's ready state: either `true` or `false`. When all
players and observers have set their ready state to `true` the game
starts.

###### "mode" < string >

Set the client's mode: either "player" or "observer". Observers can
watch the game but not interact with it. All clients start as a
"player".

###### "seed" < number >

Seed the random number generator with the supplied seed. Use this to
reproduce a model run.

##### Game Messages From the Server to the Client

The game starts when all clients signal that they are ready.

###### "start" { ... }

The start event is sent to all clients when the game starts. It
contains a snapshot of the game state.

The "player" property contains a 6 element array for each player:

    [ alive, x, y, angle, speed, turnFlag, water ]

The "map" property contains the width and height of the map, and the
entire map in a 1-dimensional array. Each element is a number
representing a map tile (forest, mountain, water, grass, etc). In
addition "map" contains the locations of all fires, fire retardant,
and waypoints.

The map's "viewPort" is the currently visible portion of the map.

##### "part" {"id": < player id >}

A player has left the game. Subsequence "update" events will not
contain information about this player anymore.

##### "update" { ... }

This is sent every 100ms. It contains information about what has
changed in the game state as well as player attributes (position,
angle, etc).

properties:

`"t"` The game tick on the server at the time of the update.

`"p"` The player attributes. See "start" for the meaning of the players'
6 element array.

`"m"` An array containing the changes to the map. Each element is a
2-element array. The first element is the map location and the second
element is the new tile ID for that location. Currently the only thing
that changes on the map is tiles that turn to ash after being burned.

`"f"` An array containing changes to the map's fire list. Each element is a
2-element array. The first element is a number (1=add, 2=delete). The
second element contains the fire object that is being added or deleted
from the map's "fire" list.

`"r"` An array containing changes to the map's retardant list. Similar to
`"f"` but for the white squares that airplanes drop to put out fires.

`"wp"` An array containing new waypoints. Similar to `"r"` and `"f"`
but for waypoints. Waypoints can only be added.

`"lms"` Stands for "last movement sequence". This contains the last
movement sequence received by the server at the time of the
update. see `"movementRequest"` below for more info.

##### "end" { ... }

The game's end condition has been met or a client has sent the "abort"
message to abort the game. The game is over and all clients return to
the lobby.

#### Messages From client to the Server

##### "abort"

The client sends this event to request a stop to the game. When the
game is aborted all clients return to the lobby.

##### "movementRequest" [ < seq >, < turn >, < thrust >, < dump >, < waypoints > ]

The client must send a movement request every game tick. The movement
request contains a 5 element array.

The first element is the sequence counter. Every movement request that
is sent, this counter goes up by one. The "update" events include the
last `seq` number that the server received at the time of the
update. This is used by the client for movement prediction, which
gives players lag-free control of their own airplane.

The second element is the turn flag. This can be `0` for no turning,
`"l"` for turning counter-clockwise, and `"r"` for clockwise.

Third element is acceleration. `0` means maintain current speed, `"f"`
means go faster, `"s"` means go slower.

Fourth element controls dumping and refilling water. `0` means do not dump
or refill and `1` means dump or refill.

The Fifth element tells the server to place waypoints at the current
location. There is currently only a water waypoint. So to place a
water waypoint at the airplane's current position, this parameter
should be `[1]`.

#### TCP interface

For models or non-browser clients it may be easier to use the TCP
interface. By default this service listens on port 3001.

Each message sent to and from the client and server has two parts: an
event and the data associated with that event. The event is always a
string and the data can be any JSON data. The end of a message is
marked with a \n newline character.

Because the \n character is used as part of the protocol, your JSON
data cannot contain the \n character in any of its strings. Luckily,
with the current wildfire protocol there is no need to send newlines
in JSON strings.

A message is an array of size 2. The first element is the event name,
a string, and the second element is the data. For example, a "roster"
event from the server would look like this:

    ["roster",{"clients":[{"id":"74530d00-eab9-11e9-9a3b-d566634f87ac","mode":"player","ready":false},{"id":"76bedce0-eab9-11e9-9a3b-d566634f87ac","mode":"player","ready":false}]}]\n

To send the "mode" event the client would send this string over the TCP socket:

    ["mode","observer"]\n

Note the \n at the end of the message. This is necessary to tell where
the message ends and the next message in the stream starts.

#### Example

The following is an example of the messages and JSON strings passed
between the client and server. Some of the repetitive communications
and large arrays have been truncated for clarity and brevity (anywhere
you see a `...`).

Lines starting with `<<<` indicate data coming from the server and
lines starting with `>>>` indicate data being sent from the client to
the server.

Each message has an event name followed by the JSON data that came
with that event.

    <<< welcome {"id":"e631e640-d3fc-11e9-9d57-d1bf25c4edab"}
    <<< roster {"clients":[{"id":"e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab","mode":"player","ready":false},{"id":"e631e640-d3fc-11e9-9d57-d1bf25c4edab","mode":"player","ready":false}],"join":"e631e640-d3fc-11e9-9d57-d1bf25c4edab"}
    >>> ready True
    <<< roster {"clients":[{"id":"e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab","mode":"player","ready":false},{"id":"e631e640-d3fc-11e9-9d57-d1bf25c4edab","mode":"player","ready":true}]}
    <<< roster {"clients":[{"id":"e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab","mode":"player","ready":true},{"id":"e631e640-d3fc-11e9-9d57-d1bf25c4edab","mode":"player","ready":true}]}
    <<< start {"id": "e631e640-d3fc-11e9-9d57-d1bf25c4edab", "map": {"width": 400, "height": 400, "viewPort": {"x": 146, "y": 146, "w": 108, "h": 108}, "retardant": [], "fire": [{"x": 163, "y": 239, "level": 1}, {"x": 155, "y": 247, "level": 1}, ...], "wayPoints": [], "data": [8, 8, 8, ...]}, "players": {"e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab": [1, 4000, 4000, 100, 0, 0, 50], "e631e640-d3fc-11e9-9d57-d1bf25c4edab": [1, 4000, 4000, 100, 0, 0, 50]}}
    >>> movementRequest [0, 0, 0, 0, []]
    <<< update {"t": 492, "p": {"e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab": [1, 4000, 4000, 100, 0, 0, 50], "e631e640-d3fc-11e9-9d57-d1bf25c4edab": [1, 4000, 4000, 100, 0, 0, 50]}, "m": [[99356, 2], [59354, 2]], "f": [[2, {"x": 156, "y": 248, "level": 1}], [1, {"x": 157, "y": 248, "level": 1}], [1, {"x": 156, "y": 249, "level": 1}], [2, {"x": 154, "y": 148, "level": 1}], [1, {"x": 154, "y": 149, "level": 1}], [1, {"x": 153, "y": 148, "level": 1}], [1, {"x": 154, "y": 147, "level": 1}]], "r": [], "wp": [], "lk": {"seq": None, "tick": None}}
    >>> movementRequest [1, 0, 0, 0, []]
    >>> movementRequest [2, 0, 0, 0, []]
    >>> movementRequest [3, 0, 0, 0, []]
    <<< update {"t": 498, "p": {"e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab": [1, 4000, 4000, 100, 0, 0, 50], "e631e640-d3fc-11e9-9d57-d1bf25c4edab": [1, 4000, 4000, 100, 0, 0, 50]}, "m": [[83446, 2], [83392, 2], [69414, 2]], "f": [[2, {"x": 246, "y": 208, "level": 1}], [2, {"x": 192, "y": 208, "level": 1}], [2, {"x": 214, "y": 173, "level": 1}]], "r": [], "wp": [], "lk": {"seq": 2, "tick": None}}
    >>> movementRequest [4, 0, 0, 0, []]

    ...

    >>> movementRequest [20, "r", 0, 0, []]
    >>> movementRequest [21, "r", 0, 0, []]
    >>> movementRequest [22, "r", 0, 0, []]
    >>> movementRequest [23, "r", 0, 0, []]
    >>> movementRequest [24, "r", 0, 0, []]
    <<< update {"t": 524, "p": {"e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab": [1, 4000, 4000, 139, 0, "r", 50], "e631e640-d3fc-11e9-9d57-d1bf25c4edab": [1, 4000, 4000, 112, 0, "r", 50]}, "m": [[85836, 2], [84639, 2], [78554, 2]], "f": [[2, {"x": 236, "y": 214, "level": 1}], [1, {"x": 236, "y": 215, "level": 1}], [1, {"x": 235, "y": 214, "level": 1}], [2, {"x": 239, "y": 211, "level": 1}], [2, {"x": 154, "y": 196, "level": 1}], [1, {"x": 153, "y": 196, "level": 1}], [1, {"x": 154, "y": 195, "level": 1}]], "r": [], "wp": [], "lk": {"seq": 23, "tick": None}}

    ...

    >>> movementRequest [80, "l", "f", 0, []]
    >>> movementRequest [81, "l", "f", 0, []]
    >>> movementRequest [82, "l", "f", 0, []]
    <<< update {"t": 594, "p": {"e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab": [1, 3950.6942949759145, 3961.022791846611, 247, 2.600000000000001, "r", 50], "e631e640-d3fc-11e9-9d57-d1bf25c4edab": [1, 3999.728190705395, 4000.1267473422067, 154, 0.2, "l", 50]}, "m": [[61353, 2], [85446, 2], [84242, 2]], "f": [[2, {"x": 153, "y": 153, "level": 1}], [1, {"x": 154, "y": 153, "level": 1}], [1, {"x": 153, "y": 154, "level": 1}], [1, {"x": 152, "y": 153, "level": 1}], [2, {"x": 246, "y": 213, "level": 1}], [1, {"x": 247, "y": 213, "level": 1}], [2, {"x": 242, "y": 210, "level": 1}], [1, {"x": 241, "y": 210, "level": 1}]], "r": [], "wp": [], "lk": {"seq": 81, "tick": None}}
    >>> movementRequest [83, "l", "f", 0, []]

    ...

    <<< part {"id": "e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab"}
    <<< update {"t": 930, "p": {"e631e640-d3fc-11e9-9d57-d1bf25c4edab": [1, 3894.7181164314875, 4141.845066656051, 220, 5, "r", 22]}, "m": [[95759, 2], [70597, 2], [71792, 2]], "f": [[2, {"x": 159, "y": 239, "level": 1}], [1, {"x": 158, "y": 239, "level": 1}], [2, {"x": 197, "y": 176, "level": 1}], [2, {"x": 192, "y": 179, "level": 1}], [1, {"x": 192, "y": 178, "level": 1}]], "r": [[1, {"x": 195, "y": 208, "amt": 1, "timeout": 993}], [1, {"x": 195, "y": 207, "amt": 1, "timeout": 985}], [2, {"x": 197, "y": 198, "amt": 1, "timeout": 929}]], "wp": [], "lk": {"seq": 359, "tick": None}}
    >>> movementRequest [361, "r", "f", 1, []]
    >>> movementRequest [362, "r", "f", 1, []]
    <<< end {}
    <<< "roster" {"clients":[{"id":"e54ae1a0-d3fc-11e9-9d57-d1bf25c4edab","mode":"player","ready":false}]}
