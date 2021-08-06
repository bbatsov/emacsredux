---
layout: post
title: Interacting with the Menu Bar
date: 2021-08-06 08:40 +0300
tags:
- Terminal
- Menu Bar
---

Interacting with the Emacs menu bar is trivial, right? You just
click some menu item and that's it. Or is it? What if you're
running Emacs in terminal mode, or you're the type of person
who doesn't like using the mouse much. Today's
short article is for you!

There are two ways to open the menu bar without using the mouse:

* Typing `M-x menu-bar-open`
* Pressing `F10` (that is the default keybinding for `menu-bar-open`)

This will expand the first menu in the menu bar (typically `File`) and allow you to
navigate around with your arrow keys. If you decide you don't want to run anything
from the menu bar you can just press `C-g` to close the menu item selection. Super simple!

Here's how this looks in terminal mode, where it is most handy:

![emacs_menu_bar.png](/assets/images/emacs_menu_bar.png)

That's all I have for you today. I know that relatively few Emacs users ever use
the menu bar (I even used to hide it completely when I was younger and my memory
worked better), but I think it's an useful way to explore available
functionality and it may come in handy from time to time.  Keep hacking!
