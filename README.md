### What Is This?

This is a cooperative game played by two people. The goal is to get as
high a score as possible in 3 minutes by destroying the "fortresses"
and avoiding the blue and red hemispheres.

Players control a tank-like vehicle in a frictionless environment that can fire missiles.

Use W to thrust

use A to rotate counter-clockwise

use D to rotate clockwise

use SPACE to fire a missile


### How To Run This Program

The program runs as a 20 game experiment. An electron app runs as the
server and listens on two different ports: 3100 for humans and 3000
for models.

1. install the required libraries:

    npm install

2. run the electron server:

    npm start

3. Connect player 1 to the server by opening a browser (I recommend
Chrome or Firefox) to the URL listed in the server window. For
instance, on my machine:

    GameServer: Server running at http://192.168.1.66:3100

4. Connect player 2 to the server by opening the same url on a second machine.

5. Each player types in a different Subject ID

### Model Support

This server has only basic model support. You can connect to the
server on port 3000 and communicate via plain-text and LISP
s-expressions. It's probably buggy.

Game state dumps provide properties such as coordinates and angles for
the in-game entities (fortresses, missiles, etc) but there is no pixel
data.

Here is an example:

    $ telnet localhost 3000
    Trying ::1...
    Connected to localhost.
    Escape character is '^]'.
    (:screen-type "config" :id "model3" :logFormat "native")
    continue
    state
    (:screen "waiting")
    state
    (103 0 ((1 0 650 650 0 0 0 0) (1 0 650 1350 0 0 0 0)) ((1 27.8 -1 -1) (1 27.8 -1 -1) (1 27.8 -1 -1) (1 27.8 -1 -1) (1 27.8 -1 -1) (1 27.8 -1 -1) (1 27.8 -1 -1) (1 27.8 -1 -1) (1 27.8 -1 -1)) () () () ())
    keydown fire
    state
    (331 0 ((1 0 650 650 0 0 0 0) (1 0 650 1350 0 0 0 0)) ((1 88.3 -1 -1) (1 88.3 -1 -1) (1 88.3 -1 -1) (1 88.3 -1 -1) (1 88.3 -1 -1) (1 88.3 -1 -1) (1 88.3 -1 -1) (1 88.3 -1 -1) (1 88.3 -1 -1)) () ((0 812 1350)) () ())
    keyup fire
    keydown fire
    keydown thrust
    state
    (1455 0 ((1 84 650 650 0 0 0 0) (1 0 761 1350 2.96 0 0 1)) ((1 28.3 -1 -1) (1 28.3 -1 -1) (1 28.3 -1 -1) (1 28.3 -1 -1) (1 28.3 -1 -1) (1 28.3 -1 -1) (1 28.3 -1 -1) (1 28.3 -1 -1) (1 28.3 -1 -1)) () () () ())
    keyup thrust
    state
    (:screen "score" :points 0 :rawPoints 0)
    quit
    Connection closed by foreign host.

### License

This game was written by Shawn Betts. It is available under the MIT
license. See the file `LICENSE` for details.
