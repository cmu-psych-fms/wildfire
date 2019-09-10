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

Players and observers can press `Escape` to disconnect from the server and stop the game updates.

### Model Interface

There is no formal model interface. It is purely an interactive, browser-based demo.

All communication is done using Socket.io over WebSocket to exchange
JSON strings between the server and clients. So it should be possible
to write a model that uses this some protocol to interact with the
server.

I have written a demo python model player in `model_client.py` that
provides more information about how one might do this. To try it you
will need the `socketio` python package and python 3:

    $ python3 model_client.py

If you'd like to see all data being sent and received from the server,
run it like this:

    $ python3 model_client.py --debug

To connect to the server using LISP and ACT-R may require more
work. As far as I know there is a Websocket library for LISP but no
Socket.io library. Using LISP to communicate directly with the
wildfire server may require implementing a large part of Socket.io in
LISP.

It may be easiest to spawn a Python script similar to model_client.py
as a subprocess from LISP and communicate with it over a pipe to send
and receive data to and from the wildfire server.

### Client/Server Communication

The following is an example of the messages and JSON strings passed
between the client and server. Some of the repetitive communications
and large arrays have been truncated for clarity and brevity (anywhere
you see a `...`).

Lines starting with `<<<` indicate data coming from the server and
lines starting with `>>>` indicate data being sent from the client to
the server.

Each message has an event name followed by the JSON data that came
with that event.

    <<< connected
    >>> greet {"mode": "player"}
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
