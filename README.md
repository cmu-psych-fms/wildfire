### What Is This?

This is a new implementation of a firefighting game based on Shawn Betts’s *wildfire*,
This new implementation being primarily written in Lisp. The game play of this new version is
significantly different from Shawn’s original game, being largely mouse driven rather than keystroke driven.

As of 21 March 2024 this remains just the beginnings of this new implementation, but incorporates an example
of what is believed to be every major component the finished version will require, and it appears to work well.
As additional features are added they will be documented in this README. In what follows I
point out many of the features I hope to implement as development progresses.

### Compatibility

This *should* be playable in any major browser released since 2018, but will likely fail in earlier browser versions.
The UI is aimed at a computer with a screen of at least 1920x1080 pixels, and with a mouse. It may or may not be
playable with a smaller screen, and almost certainly will not be playable on a phone or tablet.
As of 21 March 2024 there appears to be a bug when used Apple Safari, but we hope to understand and fix that bug soon.

The server-side code has only been tested in SBCL in Linux, but *should* run fine in any modern Common Lisp implementation
that can run USOCKET and Bordeaux Threads. It *should* be possible to run it in Windows, though some minor modifications
may be necessary; if so, this must be made by someone who understands Windows better than I do, and who has
access to a Windows machine.

### Terminology

### Starting a Game

### Game Play



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
