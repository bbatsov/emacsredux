---
layout: post
title: "Manage processes with proced"
date: 2013-05-02 14:44
comments: true
tags:
- Utilities
---

One extremely cool (but little known) new feature in Emacs 23 was the addition of `proced`.

`proced` basically allows you to run `top` inside Emacs and monitor/control processes via it. Here's
how it looks in action:

![proced](/assets/images/proced.png)

Normally you'd use `M-x proced` to start the command, but as mentioned
earlier I find it extremely useful and therefore I bind it to `C-x p`
(inspired by `dired`'s `C-x d`):

``` elisp
(global-set-key (kbd "C-x p") 'proced)
```

See `proced-mode`(`C-h f proced-mode`) for a description of features available in Proced buffers.

Some of you might have noticed that the screenshot in the post is
taken under GNOME, which is kind of strange considering I'm an OSX
user. Unfortunately `proced` does not work on OSX(but perhaps surprisingly it works on Windows).

The `C-x p` keybinding works out-of-the box on [Prelude](https://github.com/bbatsov/prelude).
