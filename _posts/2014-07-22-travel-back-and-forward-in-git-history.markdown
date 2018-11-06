---
layout: post
title: "Travel back and forward in git history"
date: 2014-07-22 19:02
comments: true
tags:
- util
---

I recently discovered an extremely cool package called
[git-timemachine](https://github.com/pidu/git-timemachine) that allows
you to step though the git history of the file you're currently
editing in Emacs.

Using it is pretty simple:

* visit a git-controlled file and issue `M-x git-timemachine` (or bind it to a keybinding of your choice)
* use the following keys to navigate historic version of the file:
    * `p` visit previous historic version
    * `n` visit next historic version
    * `w` copy the hash of the current historic version
    * `q` exit the time machine buffer

Here you can see `git-timemachine` in action:

![git-timemachine](/assets/images/timemachine.gif)

**P.S.**

This package is bundled with
[Prelude](https://github.com/bbatsov/prelude).
