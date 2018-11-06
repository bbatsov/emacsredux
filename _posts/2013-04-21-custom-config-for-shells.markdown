---
layout: post
title: "Custom config for shells"
date: 2013-04-21 15:06
comments: true
tags:
- Utilities
---

Anybody knows that you can run a shell inside Emacs with `M-x
shell`. Few people know you can have Emacs run some additional
configuration for the shell after it's started.

Emacs sends the new shell the contents of the file
`~/.emacs_shellname` as input, if it exists, where `shellname` is the
name of the name if your shell - `bash`, `zsh`, etc. For example, if you
use `bash`, the file sent to it is `~/.emacs_bash`. If this file is not
found, Emacs tries with `~/.emacs.d/init_shellname.sh`.

One popular application of such custom configuration is to have a
simpler shell prompt in Emacs, compared to the one you usually employ,
since `shell-mode` doesn't deal well with fancy prompts.

You can find out more about running interactive shells in Emacs
[here](http://www.gnu.org/software/emacs/manual/html_node/emacs/Interactive-Shell.html)
or by typing `C-h r m Interactive Shell RET` in your favorite text
editor.
