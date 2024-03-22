### What Is This?

This is a new implementation of a firefighting game based on Shawn Betts’s *wildfire*,
This new implementation being primarily written in Lisp. The game play of this new version is
significantly different from Shawn’s original game, being largely mouse driven rather than keystroke driven.
The game is intended not as entertainment but rather as a platform for experiments in psychology.
Players fly virtual airplanes over a virtual terrain extinguishing wildfires.

As of 21 March 2024 this remains just the beginnings of this new implementation, but incorporates an example
of what is believed to be every major component the finished version will require, and it appears to work well.
As additional features are added they will be documented in this README. In what follows I
point out many of the features I hope to implement as development progresses.


### Compatibility

This *should* be playable in any major browser released since 2018, but will likely fail in earlier browser versions.
The UI is aimed at a computer with a screen of at least 1920 × 1080 pixels, and with a mouse or touchpad. It may or may
not be playable with a smaller screen, and almost certainly will not be playable on a phone or tablet.
As of 21 March 2024 there appears to be a bug when used with Apple Safari, but I hope to understand and fix that bug soon.

The server-side code has only been tested in SBCL on Linux, but *should* run fine in any modern Common Lisp implementation
that can run USOCKET and Bordeaux Threads. It *should* be possible to run it in Windows, though some minor modifications
may be necessary; if so, this must be made by someone who understands Windows better than I do, and who has
access to a Windows machine.


### Terminology

Wildfire has *games*, *missions* and *players*.

A *game* brings together attributes of a particular virtual environment, or experiment. It’s most important
component is *map*, describing the terrain. The map is rectangular, and divided into square *cells*, each *cell*
being a particular type of terrain, such as grass or woodland. When defining a *map* polygonal *regions* are
defined and named, being contiguous sets of *cells* of the same type. A game has a *name* and an *id*, as well
as various other parameters such as where in the *map* the *player* starts and details of fire initiation and
propagation. There may be any number of *games* available, and they are defined before the server is started,
using `defgame` forms in `.lisp` files in the `games/` subdirectory of the main server directory. This is described
in more detail below under the heading Defining Games.

A *player* is one participant. Currently a *player* can only manipulate a single airplane, and each airplane has a
single *player* associated with it; in the future I expect to allow one *player* to manipulate multiple planes.

A *mission* is an instantiation of a game with one or more players. Currently only a single *player* is supported
in a *mission* but in the future I expect to support multiple *players* in the same *mission*, and the code
is structured in a way that is expected to support such use. The *game* defines
the initial state of the *mission*, which evolves as it is played. A *mission* is created when the user points a browser
at the Wildfire URL, as described in the next section.


### Starting a Game

If Wildfire is running on a server named `<server>` using port `<port>` game play can be started by pointing
a browser at

    http://<server>:<port>/

For example, if the server is `koalemos.psy.cmu.edu` and the port is `8978` (the default)

    http://koalemos.psy.cmu.edu:8978/

This will create a new player playing a new mission using a default game. It is possible to provide the name of an
alternative game using `?game=`. For example, to play the `model-game` game

    http://koalemos.psy.cmu.edu:8978/?game=model-game

When starting a game in this way the names of the mission and game are provided by the server. However, names
can be provided using `?mission=` and/or `?player=`, which facilitates multiple players joining the same
mission. While multiple players in a mission is not yet supported, the code exists for joining such missions
as soon as it is. Similarly, this allows the same player to participate in different missions but have their
name recorded to ease comparison of their performance. For example

    http://koalemos.psy.cmu.edu:8978/?game=conflagration&player=fred&misison=fred-and-john-mission


### Game Play

In the browser window the user is presented with a *view* into the game’s map, 39 cells by 39 cells.

![screen image](/images/screen-for-doc.png)

The player’s plane is centered in this image, and remains so as the plane flies about the landscape, with
the terrain shown varying as the plane’s position on the underlying map changes. If the plane is not moving
it should be interpreted as it circling above the location at which is is stopped. When moving the plane
currently travels at a constant speed, but in the future it will be possible to vary the speed of he plane.
The plane cannot fly off the underlying map, when it reaches the edge of the underlying map it stops.

While not yet present, in the future there will be

* indications on the border of the view showing the direction in which lie various items outside the current
  view, but still known to the player

* foo bar baZ
laj lajidsf lkj adfs


### Running the Server

Installation and running should be straightforward:

* Install [SBCL](https://www.sbcl.org/)

* Install [QuickLisp](https://www.quicklisp.org/beta/) and arrange for it to be loaded whenever SBCL is launched

* clone [this repo](https://github.com/dfmorrison/wildfire) and cd into it

* launch the server by running `./run.sh`

* point a browser at a suitable URL, as described above


### Defining Games


### Defining Models


### Mission Logs
