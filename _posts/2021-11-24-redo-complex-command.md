---
layout: post
title: Redo Complex Command
date: 2021-11-24 13:28 +0200
tags:
- Built-ins
---

I don't know about you, but from time to time I definitely have the need to
re-run some Emacs command that takes user input (e.g. `query-replace`).  For the
purpose of this article let's call such commands "complex" commands.
Obviously we can just invoke the same command, provide again the input (directly or using some parameter history if available), but it would be nice if there was
a faster way to do this.

As it's usually the case with Emacs, such a way already exists - the
built-in command `repeat-complex-command`. By default it's bound to both
`C-x M-:` and `C-x M-ESC`.[^1] When you invoke the command you'll see in the
minibuffer the last Emacs command you ran and you can use `M-n` and `M-p` to move
forward and backward in the command history.

You'll notice that the commands are listed as Emacs Lisp code that you can edit
directly if needed. You'll also notice there are no commands that don't any parameters on the list. Imagine something like:

``` emacs-lisp
(describe-key '(("\272" . [134217786])))
(occur "emacs" nil)
(projectile-ag "emacs" nil)
(markdown-insert-gfm-code-block "emacs-lisp" nil)
```

This might also give you a hint as to why the keybinding `C-x M-:` was chosen -
it's pretty close to `M-:` (`eval-expression`), which allows you to run
arbitrary Emacs Lisp code from the minibuffer.

When it comes to Emacs we're all constantly learning. I didn't know about this
command until several days ago I saw it in a
[tweet](https://twitter.com/mickeynp/status/1457826371057639429?s=20) shared by
[Mickey Petersen](https://twitter.com/mickeynp), a true Master of Emacs. Thanks
for the tip, Mickey!

[^1]: Not exactly the most convenient keybindings, right?
