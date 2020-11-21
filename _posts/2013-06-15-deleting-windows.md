---
layout: post
title: "Deleting Windows"
date: 2013-06-15 10:28
comments: true
tags:
- Utilities
---

Every Emacs user knows that he can split the current window
horizontally (with `C-x 2`) and vertically (with `C-x 3`) as much as
he desires to. However, some Emacs users don't know what to do with
the extra windows they've created when they do not them.

To delete the selected window, type `C-x 0` (`delete-window`).
Once a window is deleted, the space that it occupied
is given to an adjacent window (but not the minibuffer window, even if
that is the active window at the time). Deleting the window has no effect on the
buffer it used to display; the buffer continues to exist, and you can
still switch to with `C-x b` or any other buffer navigation command.

`C-x 4 0` (`kill-buffer-and-window`) is a stronger (and fairly
unknown) command; it kills the current buffer and then deletes the
selected window (basically it combines `C-x k` and `C-x 0`). Obviously
it's a good idea to use it on windows displaying buffers you're no
longer needing.

`C-x 1` (`delete-other-windows`) deletes all the windows, _except_ the
selected one; the selected window expands to use the whole frame.
(This command cannot be used while the minibuffer window is active;
attempting to do so signals an error.) In the era of narrow screens I
used that command fairly often when I needed to focus on a particular
task. Now I keep my screen split in half vertically 99% of the time,
but I still use `C-x 1` from time to time when I'm about to resplit my
screen in some uncommon way.

Windows displaying help buffers (generally created with commands like
`C-h ...`) warrant a special mention. They can be deleted with a
single keystroke - `q`. That would delete the help window altogether
if it was created by the help command, or restore its original
content if the window existing beforehand and was reused by the help command.
