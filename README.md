versatiler (work-in-progress)
===============================================================================

The `versatiler` will be a simple, modular tool for organising the windows on 
your desktop.

It is written because I got tired of the clunkiness of stacking WMs; of the 
assumptions that dynamic tiling WMs make; of the mental burden that manual 
tiling WMs place on the user; and of how time-consuming it is to try all of 
them only to come to this conclusion. 

At the moment, `versatiler` is only an idea. I am currently fleshing it out. 
Since it's a hobby project, work on it will be sporadic and I'm allowing for 
perfectionism to delay delivery.


Supporting programs
-------------------------------------------------------------------------------

Since `versatiler` is modular, you will need additional software to get a 
complete working environment. I can only suggest what I have tried:

-   It shouldn't really matter what underlying window manager you use, as long 
    as it's stacking, X-based, and EMWH-compliant. You may want 
    [Openbox](http://openbox.org), 
    [xfwm](https://docs.xfce.org/xfce/xfwm4/start), or 
    [windowchef](https://github.com/tudurom/windowchef).

-   To interact with `versatiler`, you will need to exchange messages with it. 
    [jq](https://stedolan.github.io/jq/) is useful for composing and parsing 
    JSON messages, and [socat](http://www.dest-unreach.org/socat/) can send 
    and receive them. Of course, you will mostly be sending straightforward 
    commands via keyboard shortcuts, so you don't need to delve into all that: 
    example commands will be included for use with the keyboard shortcut tool 
    [sxhkd](https://github.com/baskerville/sxhkd).

-   If your window manager doesn't come with a status bar, you could use 
    [polybar](https://github.com/jaagr/polybar), 
    [xfce4-panel](https://docs.xfce.org/xfce/xfce4-panel/start) or 
    [tint2](https://gitlab.com/o9000/tint2). For menu interaction, I'd use 
    [dmenu](http://tools.suckless.org/dmenu/) or 
    [rofi](https://github.com/DaveDavenport/rofi). Finally, even though it's 
    not very related to the window manager, I would like to draw attention to 
    the [lf](https://github.com/gokcehan/lf) file manager, and the 
    [xcape](https://github.com/alols/xcape) or 
    [caps2esc](https://gitlab.com/interception/linux/plugins/caps2esc) tools, 
    since they improved my workflow.


License
-------------------------------------------------------------------------------

`versatiler` is fully free and open-source software, released under GPLv3.
