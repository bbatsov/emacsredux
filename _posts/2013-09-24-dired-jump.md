---
layout: post
title: "Dired Jump"
date: 2013-09-24 17:53
comments: true
tags:
- dired
- Packages
---

Most Emacs users know that they can start `dired` (Emacs's file
browser) with `M-x dired` or `C-x d`. That would display a prompt in
the minibuffer asking which directory to open with `dired` (the
current directory will be suggested as a default).

More often than not you'll probably want `dired` to display the
directory of file you're currently editing. You might even want to
have the cursor positioned over that very same file in the `dired`
buffer. There's a cool command that does exactly that -
`dired-jump`. To use it you have to first load the built-in library
`dired-x`:

``` elisp
(require 'dired-x)
```

You can run the command with `C-x C-j` (`M-x dired-jump`). No prompts,
no wasted time. You're instantly teleported to the currently edited
file's position in a `dired` buffer.

This command works out-of-the-box in
[Prelude](https://github.com/bbatsov/prelude).
