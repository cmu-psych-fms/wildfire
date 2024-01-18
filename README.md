### What Is This?

This is the beginnings of a new implementation of a firefighting game based on Shawn Betts’s wildfire,
this new implementation being primarily written in Lisp. The game play of this new version will be
significantly different, being largely mouse driven rather than keystroke.

As of 8 September 2023 this remains just the beginnings of this new implementation.
But we’ll get more of it implemented eventually and when we do this README will become a bit more useful.

### Compatibility

This *should* be playable in any major browser released since 2018, but will likely fail in earlier browser versions.

The server-side code has only been tested in SBCL, but *should* run fine in any modern Common Lisp implementation
that can run USOCKET and Bordeaux Threads.
