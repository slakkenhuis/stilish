Planning
===============================================================================

There are seperate tasks to be done. I'll deal with things in the following 
order, leaving the rest out of consideration until time comes.

1.  IPC

    Find out how to send and receive messages to and from UNIX file sockets. 

2.  Option parsing

    Parse command-line options.

3.  Datastructures & geometry

    Write the tiling tree datastructures and calculations for positioning the 
    windows. Also write a simple box visualisation (without worrying about 
    actual terminal multiplexing!) for testing. Find a library for terminal 
    multiplexing/emulation


           ┌────2/3┬────┐
           │       │    │
           │       │    │
           │       │    │
           └───────┴────┘



4.  Window control

    Figure out how to actually control the windows.

5.  JSON parsing

    Determine the JSON DSL and write the parser for it.

6.  X-events

    Although many commands are given manually, some events should 
    automatically trigger certain actions:

    1. Opening a window attaches it to the tiling tree.
    2. Closing a window removes it from the tiling tree.
    3. Moving a window snaps it out of the tiling tree.

    Perhaps such events could be added to `sxhkd` instead. This makes sense, 
    since keypresses are also just events; this would complete the separation 
    between giving and executing commands.

7.  Finishing up

    To be determined: Things like workspaces, hiding, configuration, 
    resizing...


Resources
-------------------------------------------------------------------------------

-   Read through [Xlib](https://tronche.com/gui/x/xlib/) and 
    [XCB](https://xcb.freedesktop.org/).

-   Play with [libxdo](https://github.com/jordansissel/xdotool), 
    [libwm](https://github.com/wmutils/libwm), 
    [wmctrl](https://sites.google.com/site/tstyblo/wmctrl),
    [devilspie](https://github.com/GNOME/devilspie),
    [libwnck](https://github.com/GNOME/libwnck).

-   Other resources:

        https://zserge.com/jsmn.html
        https://www.cprogramming.com/tutorial/c-tutorial.html
        https://www.gnu.org/software/libc/manual/html_node/Sockets.html
        https://github.com/jichu4n/basic_wm
        https://www.freedesktop.org/wiki/Software/Xephyr/
        https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html
        https://xcb.freedesktop.org/
        https://github.com/enn/xcb-examples/blob/master/README.md
        https://hackage.haskell.org/package/socket-0.8.0.1/docs/System-Socket.html
        https://github.com/VyacheslavHashov/haskell-socket-unix
        https://seasonofcode.com/posts/how-x-window-managers-work-and-how-to-write-one-part-i.html
        https://tronche.com/gui/x/xlib/window/configure.html
        http://hackage.haskell.org/package/X11
        https://wiki.haskell.org/X_window_programming_in_Haskell
        https://github.com/jordansissel/xdotool
        http://www.semicomplete.com/files/xdotool/docs/html/xdo_8h.html
        https://wiki.haskell.org/Xmonad/Frequently_asked_questions
        https://wiki.haskell.org/FFI_Introduction
        https://en.wikibooks.org/wiki/Haskell/FFI
        https://www.microsoft.com/en-us/research/video/taste-haskell-part-2/
        https://hackage.haskell.org/package/rosezipper-0.2/docs/Data-Tree-Zipper.html
        https://wiki.haskell.org/Zipper_monad/TravelBTree
        https://askubuntu.com/questions/1010276/can-i-act-on-the-event-that-a-window-opens-without-polling
        https://tronche.com/gui/x/xlib/
        https://tronche.com/gui/x/xlib-tutorial/
        http://xopendisplay.hilltopia.ca/2009/Jan/Xlib-tutorial-part-1----Beginnings.html
