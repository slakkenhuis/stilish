stilish (work-in-progress)
===============================================================================

`stilish` is — or rather *will be* — a simple, modular tool for organising the 
windows on your desktop.

It is being written because I got tired of the clunkiness of stacking WMs; of 
the assumptions that dynamic tiling WMs make; of the mental burden that manual 
tiling WMs place on the user; and of how time-consuming it is to try all of 
them only to come to this conclusion. 

At the moment, `stilish` is only an idea. I am currently fleshing it out by 
writing a [proposal](doc/proposal.md). Since it's a personal hobby project, 
I'm more concerned with 'getting it right' than with delivering — I will work 
on it very sporadically, in my free time. Moreover, it is exhausting even to 
*look* at the documentation for X; I'll be happy if I can achieve only part of 
my proposal. Therefore, unless other developers take an interest, it will 
probably take a while before the actual program is finished.



Supporting programs
-------------------------------------------------------------------------------

Since `stilish` is quite modular, you will want additional software to get a 
complete working environment. I can only suggest what I have tried:

-   It shouldn't really matter what underlying window manager you use, as long 
    as it's stacking, X-based, and EMWH-compliant. You may want 
    [Openbox](http://openbox.org), 
    [xfwm](https://docs.xfce.org/xfce/xfwm4/start), or 
    [windowchef](https://github.com/tudurom/windowchef).

-   To interact with `stilish`, you will need to exchange messages with it.  
    [jq](https://stedolan.github.io/jq/) is useful for composing and parsing 
    JSON messages, and [socat](http://www.dest-unreach.org/socat/) can send 
    and receive them. Of course, you will mostly be sending straightforward 
    commands via keyboard shortcuts, so you don't need to delve into all that: 
    example commands will be included for use with the keyboard shortcut tool 
    [sxhkd](https://github.com/baskerville/sxhkd). For X event hooks, I still 
    need to find a program. Perhaps I should write it myself, since it needs 
    special knowledge (e.g. if a window is moved by the user, it should be 
    automatically unmanaged).

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

`stilish` is fully free and open-source software, released under GPLv3.
