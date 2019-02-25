---
title: "Proposal for `stilish` window manager manager"
---

General philosophy
===============================================================================

-   As it is for [i3](https://i3wm.org/), user-friendliness and documentation 
    are a primary focus.

-   Like [bspwm](https://github.com/baskerville/bspwm), the program is very 
    modular. All communication — be it commands, configuration or querying — 
    is done by sending and receiving JSON to a UNIX file socket. Nothing like 
    a status bar or even keyboard handling is included — the only real 
    functionality is window arrangement.

-   Still in pursuit of modularity, `stilish` is not meant to be used as a 
    standalone window manager. Instead, it is a layer to be applied on top of 
    your favourite stacking WM. (See also 
    [pytyle](https://github.com/BurntSushi/pytyle1), 
    [pytyle3](https://github.com/BurntSushi/pytyle3).) Whatever configuration 
    you had set up already — you can probably keep it. (A more minimalist, 
    monolithic WM with similar tiling behaviour could be made later, and the 
    code should anticipate that; but it would be a separate program that 
    merely shares some of the codebase.)

-   Only the developer should ever have to think about the structure of the 
    tiling tree. For the user, the movements should feel obvious.

-   To reduce the mental load, it helps to have as few keyboard shortcuts as 
    possible. So, for example, resizing is an operation for which you only 
    need to remember a modifier and the directional keys: the first keypress 
    will grab an edge, and subsequent keypresses will move that edge around.

-   There should be a balance between manual and automatic operations: doing 
    too much manually would defeat the purpose of a tiling manager, whereas 
    doing too much dynamically can be frustrating. Everything that can be done 
    automatically should be optional, and should also be possible manually.

-   Ideally, tabs would be exclusively handled by the window manager, but that 
    is not how applications are designed now. We therefore will skip tab 
    support entirely. What we *will* have is window stacking, because it is 
    often useful to have multiple windows occupying the same space. The 
    windows (or frames) can be rotated through, as in a carousel.

-   I have not yet decided whether to use X or Wayland, and which programming 
    language to use. From what I can see, Wayland will have to be monolithic 
    since I would have to change the whole compositor. I think I will use C 
    for the X iteration, and Haskell or another high-level language if I will 
    ever start a Wayland version.



Behaviour
===============================================================================

Tiling tree
-------------------------------------------------------------------------------

-   Binary tiling is too unintuitive for mildly complex layouts, so the tree 
    will be a *rose tree*. Each workspace will have its own tree.

-   *Internal* nodes contain an *orientation* indicating whether its children 
    should be laid out vertically ('on top of eachother' - with horizontal 
    lines between) or horizontally ('next to eachother' - with vertical lines 
    between). *External* nodes contain a singular window. Every node also has 
    its size: either in absolutes or as a ratio relative to the rest of the 
    frame. As for an intuitive name for the external and internal nodes of the 
    tree, I suggest 'window tiles' and 'framing tiles', respectively.

-   For flexibility, a third orientation is added: for those that are 
    *stacked* on top of eachother. In this way, we get a '3D'-desktop: we can 
    move horizontally, vertically, or depthwise. A stack of windows is a 
    *carousel* if the end of the stack is connected to its beginning. 

-   I have not yet decided whether stacks can only contain windows, or if they 
    can contain frames, too. In the former case, they occur only one layer 
    above the leaf nodes, which is more restrictive but easier to handle in 
    terms of permitted movement. The second case would need some thorough 
    thinking, because it means we may have multiple layers of depth.

-   Initially, the idea was to sacrifice versatility for simplicity, and allow 
    stacking only on leaf nodes and have a tiling tree that only alternates 
    between horizontal and vertical. However, I soon figured out that's not 
    even easier to reason about. Instead, every window frame can be vertical, 
    horizontal or stacked in orientation, with the following constraints:

    -   Two successive levels can not have the same orientation.

    -   Any internal node (except the root node) *must* have two or more 
        children.

    These constraints will be guaranteed to be satisfied by auto-collapsing 
    rules:

    -   A frame consisting of a single window should automatically collapse 
        into a single window.
        
    -   If a stack can contain *frames* of multiple window, as opposed to 
        singular *windows*, it *could* also be auto-collapsed.

-   Fibonacci tiling is a popular dynamic tiling approach, wherein new windows 
    are opened in the following progression:

        ┌─────┐  ┌──┬─┐  ┌──┬─┐  ┌──┬─┐
        │     │═>│  │ │═>│  ├─┤═>│  ├┬┤
        └─────┘  └──┴─┘  └──┴─┘  └──┴┴┘
    
    I have found that this does not work too well for me. Although it should 
    still be an option in `stilish`, I would personally prefer that any 
    additional windows are added as a single array of windows next to the main 
    window, either vertically or horizontally, as shown below. Different 
    layouts are achieved by manually dragging your windows to the desired 
    spot.

        ┌─────┐  ┌───┬─┐  ┌───┬─┐  ┌───┬─┐
        │     │  │   │ │  │   │ │  │   ├─┤
        │     │═>│   │ │═>│   ├─┤═>│   ├─┤
        └─────┘  └───┴─┘  └───┴─┘  └───┴─┘
           ║     ┌─────┐  ┌─────┐  ┌─────┐
           ║     │     │  │     │  │     │
           ╚════>├─────┤═>├───┬─┤═>├─┬─┬─┤
                 └─────┘  └───┴─┘  └─┴─┴─┘

-   There should be a 'void' window that can be inserted anywhere, that is 
    simply the absence of a window that does take up space.

Managing windows
-------------------------------------------------------------------------------

-   Each workspace may have *managed* and *unmanaged* windows. The former 
    category consists of those windows which are attached to a tiling tree and 
    which obey its rules. Those in the latter category behave as much as 
    possible as if `stilish` weren't running at all: they are floating and can 
    be minimised and maximised.

-   To keep a clear distinction, managed windows are always stacked lower than 
    unmanaged windows (unless they are minimised). In a way, unmanaged windows 
    are like the messy stack of papers on the worktop. Moreover, there should 
    be a quick way to minimise or un-minimise all these windows, and to make 
    them sticky.

-   Similarly, a window should never obscure any other window in *fullscreen* 
    mode. Therefore, if a window is made fullscreen, it should be moved to a 
    private workspace beforehand. The 'path' to its previous location should 
    be memorized so that it can be put back as close to its original location 
    as possible when full-screen is toggled off again.

-   There should be a mechanism for automatically managing or releasing 
    windows from the `stilish` tiling tree when triggered by X events. In 
    particular:

    -   It should be possible to automatically attach newly opened windows to 
        the tiling tree. In that case, should dialog windows be ignored? 
        Should certain other classes of windows be excluded?

    -   Only unmanaged windows can be *minimised* or *maximised* normally. 
        When you attempt to do that to a managed window, it should be 
        automatically *detached* from the tiling tree.

    -   Perhaps when a window is moved by the user but not through `stilish`, 
        it should snap out of control as well. Conversely, hovering an 
        unmanaged window over a managed window for some time might snap it 
        into the tiling tree.



Movement and focus
-------------------------------------------------------------------------------

-   Movement, focus and window insertion are always defined with respect to 
    some reference frame, or 'anchor'. The anchor can be the current window, 
    its parent frame, an adjacent window or frame, the n'th unmanaged window, 
    a path to a wholly different window, a selector that selects a window so 
    that you can emulate Fibonacci-style tiling… The position is then 
    determined by keywords like these:

        -   Up/north, down/south, left/west, right/east

        -   Stacked front/back (front or back)
    
        -   Crosswise/turned (front or back)

        -   Lengthwise/adjacent/parellel (front or back)

-   There is a multitude of different types of movement one might imagine. The 
    movements get further complicated once we consider multiple depth layers. 
    In the following cases, the focused window - the one with the dotted 
    outline - is moved to the right:

    -   **Pushing**: Removing the window from the current frame and inserting 
        it into the adjacent window or frame. Note that if the adjacent object 
        is a frame, then there are still different ways to adjust the ratios 
        of the sizes of its children; and if the adjacent object is a window, 
        then we can either revert to skipping, or bundle the two windows into 
        a new frame.

            ┌┄┄┄┄┬──┬──┐    ┌┄┄┄┄┄┄┄┬──┐
            ┊    ┊  │  │    ├┄┄┄┄┄┄┄┤  │
            ┊    ├──┤  │ => ├───────┤  │
            ┊    ┊  │  │    │       │  │
            └┄┄┄┄┴──┴──┘    └───────┴──┘

    -   **Skipping**: Move the window over the adjacent window or frame.

            ┌┄┄┄┄┬──┬──┐    ┌──┬┄┄┄┄┬──┐
            ┊    ┊  │  │    │  ┊    ┊  │
            ┊    ├──┤  │ => ├──┤    ┊  │
            ┊    ┊  │  │    │  ┊    ┊  │
            └┄┄┄┄┴──┴──┘    └──┴┄┄┄┄┴──┘

    -   **Swapping**: Keep the layout intact and just swap the contents of the 
        windows.

            ┌┄┄┄┄┬──┬──┐    ┌────┬┄┄┬──┐
            ┊    ┊  │  │    │    ┊  ┊  │
            ┊    ├──┤  │ => │    ├┄┄┤  │
            ┊    ┊  │  │    │    │  │  │
            └┄┄┄┄┴──┴──┘    └────┴──┴──┘

    -   **Stacking**: Position the whole window over or under the adjacent 
        frame, in a carousel. 

            ┌┄┄┄┄┬──┬──┐    ┌┄┄┄┄┐  ┌──┐    ┌────┐  ┌──┐
            ┊    ┊  │  │    ┊    ┊┐ │  │    ├────┤┐ │  │
            ┊    ├──┤  │ => ┊    ┊┤ │  │ or │    │┊ │  │
            ┊    ┊  │  │    └┄┄┄┄┘│ │  │    └────┘┊ │  │
            └┄┄┄┄┴──┴──┘     └────┘ └──┘     └┄┄┄┄┘ └──┘

    -   **Dealing**: Like stacking, but rather than positioning the window 
        over the whole frame, position it over its first window.

            ┌┄┄┄┄┬──┬──┐    ┌┄┄┄┄┐  ┌──┐
            ┊    ┊  │  │    └┄┄┄┄┘┐ │  │
            ┊    ├──┤  │ =>  └────┘ │  │
            ┊    ┊  │  │    ┌─────┐ │  │
            └┄┄┄┄┴──┴──┘    └─────┘ └──┘



Resizing and appearance
-------------------------------------------------------------------------------

-   The window size can be tracked in absolutes - so and so window has this 
    many pixels - but that is already computed by X itself, and calculating 
    with those sizes is difficult because changes to one window effect changes 
    to other window sizes, too; and changes in orientation are difficult to 
    calculate. Therefore, it probably makes sense to keep track of *ratios* 
    instead: in a frame with three windows of size 1, 2 and 3, the windows 
    take up 1/6, 1/3 and 1/2 of the length of the frame, along the orientation 
    of said frame.

    When a window is attached, it is possible to either 'squeeze it in' and 
    change the sizes of all the windows in the same frame, too; or to only 
    change the size of one particular window that is displaced by the new 
    window.

-   The initial ratio for a newly attached window can be configured.

-   There are user-configurable margins (right, left, top, bottom) around the 
    tiling tree. The space between windows is also user-configurable, as are :
    the x- and y-offsets of the stacking windows.

-   Changing focus should be clear, without being obtrusive. Perhaps it is a 
    good idea to temporarily draw an outline on a newly focused window that 
    quickly fades out.

-   Optionally, windows that are attached to the tiling tree should be 
    *chromeless* so as to save on screen real estate.

-   The focused frame in a stack might be shown in different ways. It is 
    probably most pleasing to 'rotate' through the windows so that the focused 
    window is on top.

-   Resizing can be achieved in multiple ways:

    -   Straightforwardly adding or subtracting a number of pixels from a 
        window and calculating how the edges move. We will not do this, 
        because it is not intuitive.

    -   Moving an edge. We can do this in a roundabout way (have keyboard 
        shortcuts for every move) or a more intuitive way: first grabbing an 
        edge, then using the same keys to move it in different directions.

-   As mentioned before, window frames will be automatically collapsed into 
    their parents if the parents have the same orientation, or if the frame 
    contains only one window. Moreover, frames can be manually collapsed into 
    their parents by the user. 
    
    When collapsing, the ratio of the siblings should be multiplied with the 
    total ratio of the old nodes. For example, in a tiling tree with three 
    top-level frames, where each number represents the ratio available: 

         / | \          / | | \
        1  2  3    =>   3 4 2 9
          / \
          2 1



Workspaces and desktops
-------------------------------------------------------------------------------

-   Although I like the idea of naming workspaces (like `i3`) or tagging 
    windows (like `dwm`), in practice I tend to create and destroy my desktops 
    on the spot. So I will switch to workspaces using their index number. 
    Non-existent workspaces can be created and empty workspaces destroyed 
    automatically.



Technicalities
===============================================================================

Data structure
-------------------------------------------------------------------------------

Every node has a pointer to its first and last child, and to its previous and 
next sibling. Every internal node represents a frame, which contains an 
orientation (horizontal, vertical or stacked). Every external node represents 
a window.

         H
      ┌─┬┴┐
      1 2 V         ┌1┐┌2┐ ┌5┐┌6┐
         ┌┴┐        │ ││ │┌───3┐┘
         S 4   =>   │ ││ │└────┘
        ┌┴┐         │ ││ │┌───4┐
        3 H         └─┘└─┘└────┘
         ┌┴┐
         5 6


JSON ideas
-------------------------------------------------------------------------------

commands: focus, resize, move, swap, attach/detach=manage/unmanage, focus 
workspace, make workspace, destroy workspace, name workspace, move to 
workspace, hide unmanaged, focus tiled/untiled, sticky window,

configure: auto-create workspaces, auto-destroy workspaces, window gap in 
pixels, margin-{top,bottom,right,left}, snap on move/minimize,maximize, 
atuo-hide unmanaged window upon focusing managed windows, auto sticky floating 
windows, auto-manage classes of windows

query: tree, window, status (tiled/floating/minimized/maximized)

