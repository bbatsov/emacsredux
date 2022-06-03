---
layout: post
title: Enable Mouse Support in Terminal Emacs
date: 2022-06-03 07:32 +0300
---

I've started writing this article 1 year ago and for some reason I never finished. Today I'm changing this![^1]

Some text terminals support mouse clicks in the terminal window (shocking, right?). Some Emacs users would like to use the mouse in terminal Emacs. This article is for them.

TLDR;

Most people (except GNU/Linux users) should add this to their `init.el`:

``` emacs-lisp
(xterm-mouse-mode 1)
```

GNU/Linux users should add this to their `init.el`:

``` emacs-lisp
(gpm-mouse-mode 1)
```

You might want to wrap this config in some check that you're actually running terminal Emacs (e.g. `emacs -nw`):

``` emacs-lisp
(unless (display-graphic-p)
  (xterm-mouse-mode 1))
```

Now let's expand on this a bit.

In a terminal emulator which is compatible with `xterm`, you can use `M-x xterm-mouse-mode` to give Emacs control over simple uses of the mouse - basically, only non-modified single clicks are supported. Newer versions of `xterm` also support mouse-tracking. The normal `xterm` mouse functionality for such clicks is still available by holding down the `Shift` key when you press the mouse button. `xterm-mouse-mode` is a global minor mode.

In the console on GNU/Linux, you can use `M-x gpm-mouse-mode` to enable mouse support. You must have the [gpm](https://wiki.archlinux.org/title/General_purpose_mouse) server installed and running on your system in order for this to work. Note that when this mode is enabled, you cannot use the mouse to transfer text between Emacs and other programs which use GPM. This is due to limitations in GPM and the Linux kernel.

Obviously `xterm-mouse-mode` will work on GNU/Linux system, but you'll get better results with `gpm-mouse-mode`.

[^1]: Probably because I almost never use the mouse in Emacs. Or because I'm very lazy. Or because I have this tendency to start more things than I can finish.
