### What Is This?

This is a new implementation of a firefighting game based on Shawn Betts’s *wildfire*,
this new implementation being primarily written in Lisp. The game play of this new version is
significantly different from Shawn’s original game, being largely mouse driven rather than keystroke driven.
The game is intended not as entertainment but rather as a platform for experiments in psychology.
Players fly virtual airplanes over a virtual terrain extinguishing wildfires.

As of 2 Decenber 2025 this remains a work in progress, but incorporates an example
of what is believed to be every major component the finished version will require, and it appears to work well.
As additional features are added they will be documented in this README. In what follows I describe the current
state of the game, but also point out many of the features I hope to implement as development progresses.
My goal throughout developing this is to keep it in a state suitable for running throughout, simply adding
features incrementally.


### Compatibility

This *should* be playable in any major browser released since 2018, but will likely fail in earlier browser versions.
The UI is aimed at a computer with a screen of at least 1680 × 1050 pixels, and with a mouse or touchpad. Efforts have
been made to keep it playable on smaller screens and/or touchscreen devices such as tablets or phones, but they are not the primary target, and
interaction on those devices may be suboptimal.
Even if it is playable on these
latter devices it is not recommended that serious data collection be done using them as human performance will probably be
degraded compared to using the recommended screen size and/or pointing technology.
As of 2 December 2025 there appears to be a bug when used with Apple Safari that will need some deep investigation to repair.

The server-side code has only been tested in SBCL on Linux, but *should* run fine in any modern Common Lisp implementation
that can run USOCKET and Bordeaux Threads. It *should* be possible to run it in Windows, though some minor modifications
may be necessary, for example dealing with files; if so, this must be made by someone who understands Windows better than I do, and who has
access to a Windows machine.


### Terminology

Wildfire has *games*, *missions* and *players*.

A *game* brings together attributes of a particular virtual environment, or experiment. Its most important
component is *map*, describing the terrain. The map is rectangular, and divided into square *cells*, each *cell*
being a particular type of terrain, such as grass or woodland. When defining a *map* polygonal *regions* are
defined and named, being contiguous sets of *cells* of the same type. A game has a *name* and an *id*, as well
as various other parameters such as where in the *map* the *player* starts and details of fire initiation and
propagation. There may be any number of *games* available, and they are defined before the server is started,
using `defgame` forms in `.lisp` files in the `games/` subdirectory of the main server directory. This is described
in more detail below under the heading [Defining Games](#defgame).

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

For example, if the server is `mneme.lan.cmu.edu` and the port is `8978` (the default)

    http://mneme.lan.cmu.edu:8978/

This will create a new player playing a new mission using a default game. It is possible to provide the name of an
alternative game using `?game=`. For example, to play the `model-game` game

    http://mneme.lan.cmu.edu:8978/?game=model-game

When starting a game in this way the names of the mission and game are provided by the server. However, names
can be provided using `?mission=` and/or `?player=`, which facilitates multiple players joining the same
mission. While multiple players in a mission is not yet supported, the code exists for joining such missions
as soon as it is. Similarly, this allows the same player to participate in different missions but have their
name recorded to ease comparison of their performances. For example

    http://mneme.lan.cmu.edu:8978/?game=conflagration&player=fred&misison=fred-and-john-mission


### Game Play

In the browser window the user is presented with a *view* into the game’s map, 39 cells by 39 cells, with furthr information
an affordances on either side of it.

![screen image](/images/screen-for-doc.png)

The player’s plane is centered in this *view*, and remains so as the plane flies about the landscape, with
the terrain shown varying as the plane’s position on the underlying map changes. If the plane is not moving
it should be interpreted as it circling above the location at which is is stopped.
The plane cannot fly off the underlying map, when it reaches the edge of the underlying map it stops.
While not yet present, in the future there will be indicators on the border of the *view* showing the direction in which lie various
items outside the current view, but still known to the player,

The user interface for declaring what a mouse click in the *view* should do may evolve in the future, but currently allows moving to
a specific location that is currently visible, or placing a marker at such a location.
To fly to locations not in the current
view you must first click on a location in the correct location and then click again as new terrain comes into view.
This will be augmented to allow flying in a direction off the view, as well as flying to any of the markers, including those
not visible in the *view*.
While clicking on a location while the plane is en route currently immediately changes the plane’s
destination, in the future it will instead be possible to queue up a sequence of destinations.

In the view there are currently seven types of cells that might be present

1. ![grass](/images/grass.png) grass
2. ![ash](/images/ash.png) ash
3. ![water](/images/water.png) water
4. ![tree](/images/tree.png) tree
5. ![road](/images/road.png) road
6. ![rock](/images/rock.png) rock
7. ![house](/images/house.png) house

In the future there may be other types of cells, too.

In addition, a grass, tree or house cell might be on fire, in which case it is augmented with flames
![flames](/images/flame.png).

A game designates one or more cells that are ignited at specific times after the game begins.
These fires then slowly spread stochasticly to neighboring flammable cells. Fires also stochasticly burn
themselves out, the burned out cells changing to ash. Fires cannot propagate across roads, water, rocks or ash.

The goal of the game is for the player(s) to find and extinguish these fires. The mechanism for extinguishing a fire is clicking on an inflamed cell to tell the
plane to fly there and extinguish all inflamed cells within a certain radius of that clicked on.
In the future this mechanism will become more complex, with the player(s) needing
to explicitly drop an extinguishing agent on a fire. A plane will only carry a limited quantity of such an agent, and will
need to refill it periodically. Planning and coördinating such use of an extinguishing agent and the required travel will
be an important part of the players efforts.


### Running the Server

Installation and running should be straightforward:

* Install [SBCL](https://www.sbcl.org/)

* Install [QuickLisp](https://www.quicklisp.org/beta/) and arrange for it to be loaded whenever SBCL is launched

* clone [this repo](https://github.com/dfmorrison/wildfire) and cd into it

* launch the server by running `./run.sh`

* point a browser at a suitable URL, as described above


### <a name="defgame"></a>Defining Games

TODO document duration

The available games are defined using the `defgame` form.

    (defgame <name> (*key <keyword arguments>) &rest <region descriptions>)

None of the arguments to `defgame` is evaluated.

A `<region description>` is of the form

    (<region type> (<region name> &key <keyword arguments>) & rest <points>)

A region is a contiguous set of cells of of the same cell type.
The currently supported region types and their cell types are
* `forest tree`
* `lake water`
* `river water`
* `road road`
* `outcrop rock`
* `houses house`
There may be further region types in the future.

The `<points>` portion of the region description should be list of an even number of positive integers, the coördinates
in cell units defining a region. Most region types are polygons, the points being their vertices, which form an implicitly
closed loop. Two of the region types,
`river` and `road` are instead paths, collections of line segments, one cell wide, with the points being points
between which these segments extend, and which typically do not form a closed loop.

An important keyword argument to `defgame` is `:ignitions`, which is a list of plists, the elements of this plist being
`:x``, `:y` and `:t`, denoting the coördinates of a cell at which a fire will start, and the time, in seconds after
the mission starts, that that fire will ignite. If no time is given, the previously specified time is re-used.

Another keyword argument to `defgame` is `:model`, which should be a symbol naming a *model*, described in the next
section, to be used for the mission playing this game.

Other keyword arguments to `defgame` include the following, which all have reasonable default values if not
explicitly supplied
* `:width` and `:height`, the dimensions, in cells, of the game’s map
* `start-x` and `start-y`, the position, in cell coördinates, at which the player’s plane starts
* `fire-exhaustion-probability` and `fire-propagation-probability`, real numbers between 0 and 1 defining how
fires spread.

In the future, when multiple players are supported, much of this
syntax will necessarily have to change, but for now it is sufficient.
Also in the future it will be possible for a game definition to inherit from another game definition, simply updating
those parts of its definition that are to change.

Here is an example of a game definition,

    (defgame model-game (:model test-model
                         :ignitions ((:x 55 :y 50 :t 4)
                                    (:x 90 :y 90 :t 7)
                                    (:x 91 :y 98)
                                    (:x 25 :y 20)
                                    (:x 64 :y 47 :t 14)))
             (forest (sherwood-forest) 0 0  30 0  45 45  10 40  0 25)
             (lake (loch-ness) 55 55  80 65  70 75  60 68  45 60)
             (river (nile) 70 0  60 20  64 60)
             (road (lincoln-highway) 40 0  4 99)
             (outcrop (bear-rocks) 45 44  49 46  47 51  46 49)
             (houses (levittown) 61 45  69 45  69 49  61 49))

To ease the addition of games to Wildfire when the server starts it looks in the `games/` subdirectory of the
server directory when the server starts. For each `.lisp` file it finds in that subdirectory it calls `load` with the
filename but not file type to load the file. Note that this means if there is a more recent compiled version of the file
it will typically load that in preference to the source file, and will otherwise load the source file. These files
should typically contain one or more `defegame` forms.

The default `games/` subdirectory contains a file, `test-games.lisp`, defining two simple example games,
`test-game` and `model-game`. These two games are identical, except that the latter uses an example model
which the former does not.


### Defining Models

In addition to human players, there can, though do not have to be, mechanical players, *models*. Once multiple players
in a single mission are supported it will be possible to have both humans and models playing in a single mission.

A *model* is implemented as a Lisp function of two arguments. This function is called each time the server and client
interact and exchange information, currently at a rate of approximately 1 Hz. Both arguments are plists describing the
then current state of the mission from this modeled players perspective.
The first argument is the “public” state of the mission; that is, the information visible to a human player.
The second argument is the “private” state, information available within the system, but not normally visible
to a human player, such as the cells on the portions of the map not currently visible; a disciplined model will
typically try not to use the private information, but it may be useful as an expedient or for special purposes.
The model can simply learn from the
information supplied in the arguments, or it can decide to take one or more actions. To take actions, the model function should return a plist,
describing the action or actions to be taken; if it takes no actions it should return `nil`.

To ease the addition of models to Wildfire when the server starts it looks in the `models/` subdirectory of the
server directory when the server starts. For each `.lisp` file it finds in that subdirectory it calls `load` with the
filename but not file type to load the file. Note that this means if there is a more recent compiled version of the file
it will typically load that in preference to the source file, and will otherwise load the source file. These files
should typically contain one or more model functions.

As of right now a game uses a model function for the player in a mission playing this game if the `defgame` form
cites the function in a `:model` keyword argument. When support for multiple players in a mission is supported
this mechanism will necessarily change slightly, but will be similar.

Among the items in the first plist argument supplied to a model function are

* `:time`, the current model time, in seconds, where zero was the start of the game
* `angle`, the orientation of the plane, in radians
* `speed`, the current speed of the modeled player’s plane, a real number between 0 and 1 inclusive,
the fraction of maximum possible speed
* `view`, a two dimensional array, each element corresponding to a cell that is currently visible to the player;
each element is either `nil`, denoting a cell of the view that is off the map and cannot be visited, or a two
element list, a keyword denoting the type of cell (*e. g.* :grass or :rock) and a Boolean indicting whether the
cell is currently on fire
* `:center`, a two element list the position in the view that the plane occupies; this is essentially a constant
that could easily be computed from the dimensions of the view array, but is supplied as a convenience.
* `:regions`, a list of alists, the car of each element being a type of cell, as above, and the cdr being a list
of cell coördinates of all the cells that constitute a contiguous region in the view of cells of the same type;
note that two different regions may, if the location of the view changes, coalesce into a single region

Here’s an example:

    (:time 33161
     :angle 2.884269
     :center (19 19)
     :speed 0.25
     :regions ((WILDFIRE:ROAD (0 0) (0 1) (0 2) ...)
               (WILDFIRE:GRASS (0 38) (0 37) (1 17) ...)
               (WILDFIRE:GRASS (21 21) (22 21) (21 22) ...)
               (WILDFIRE:TREE (2 16) (1 16) (1 15) ...) ...)
     :view #2A(((WILDFIRE:ROAD NIL) (WILDFIRE:ROAD NIL) (WILDFIRE:ROAD NIL)
                (WILDFIRE:ROAD NIL) ...)
               ((WILDFIRE:GRASS NIL) (WILDFIRE:GRASS NIL) (WILDFIRE:GRASS NIL)
                (WILDFIRE:GRASS NIL) ...)
               ((WILDFIRE:GRASS NIL) (WILDFIRE:GRASS NIL) (WILDFIRE:GRASS NIL)
                (WILDFIRE:GRASS NIL) ...)
               ((WILDFIRE:GRASS NIL) (WILDFIRE:GRASS NIL) (WILDFIRE:GRASS NIL)
                (WILDFIRE:GRASS NIL) ...)
               ...)

In the future more information will be supplied as part of this argument.

TODO document the private information supplied

The only currently supported action that the model can return is motion to a target location in the view.
To do this return `(:target <x> <y>)`, where `<x>` and `<y>` are the indices into the view array of the cell
to which to move. In the future a wider repertoire of possible actions will be available. There will also be
information available about, and actions involving, whole regions (as defined in `defgame`) identified by their names.

And example model function is in `models/test-model.lisp`. This simple example waits five seconds after the mission
starts, and then

* if the player is moving, does nothing

* if the player is stopped it looks for cells in its view that are on fire, and if it finds any that it
is not already directly on top of it picks one at random and moves to it

* if the player is topped and no such fires are found it selects a cell at random that is at least three cells
away from the current position and moves to it.

This example model is used by the example `model-game` defined in `games/test-games.lisp`.


### Mission Logs

Each mission writes a detailed description of what has transpired in the mission into a file in the `mission-logs/`
subdirectory of the main server diretory. This file has a name of the form `mission-<id>-log.lisp` where `<id>` is
replaced by the mission ID, the string representation of a
[UUID](https://datatracker.ietf.org/doc/html/rfc4122), for example

    mission-logs/mission-03F3D280-E7E2-11EE-A371-64006A6189B3-log.lisp

The primary purpose of these log files is to record detailed information about the interrelations of users with
Wildfire for subsequent analysis. While not yet implemented, it is expected also eventually to be possible to play back these log
files as a way to observe previous game play as an aid to such analysis.

A mission log file contains a sequence of textual representations of Common Lisp lists. The first element (car) of each
list is a Common Lisp keyword describing the kind of record it represents, one of the following

    :metadata
    :update-request-from-client
    :call-model
    :model-response
    :update-from-server

The second element (cadr) of each list is an [ISO 8601 timestamp](https://www.iso.org/iso-8601-date-and-time-format.html),
the time on the server that this record was written.

The remainder of the list (cddr) is a plist containing detailed information appropriate to a record of the given kind.

The first record in ever file is a `:metadata` record recording information about the mission, including, for example,
the version number of the Wildfire software, the version number of the log file format itself, the mission ID
and the name of the game being played. The timestamp in this record is also the start time of the mission. This is
the only `:metadata` record in the file, and is always first.

Each time the client and server exchange information both an `update-requiest-from-client` and an `update-from-server`
are written, the first when the server receives the triggering update request from the client, and the second when
the server responds. The first contains information the client sends to the server with its request, such as if the user
has clicked the mouse and where, and the second the information the server returns to the client, such as whither
to move and how to update whatever fires may be burning.

If a model is being used, between these two records may also appear a `:call-model` record, containing all the information
provided to the model as it is called. If the model returns a non-nil response this will be followed by a
`mofrl-trdpondr` record containing that response.

Mission log files can grow large quickly, on the order of Gigabytes, especially if a model is in use.
They do, however, compress well. It is expected that eventually on mission termination the corresponding log
file will be automatically compressed.


### License

This new version of Wildfire is released under the MIT License:

> Copyright 2024–2025 Carnegie Mellon University
>
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> “Software”), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
> NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
> LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
> OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
> WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
