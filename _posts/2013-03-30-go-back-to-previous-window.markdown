---
layout: post
title: "Go back to previous window"
date: 2013-03-30 18:11
comments: true
tags:
- Utilities
---

Every Emacs user knows what `C-x o`(`other-window`) does - it moves
your cursor to the next window. When you're out of windows - the
command will take you back to the first one.

Relatively few people are aware that the command takes a prefix
argument which allows you to move several windows forward or
backward. That's obviously pretty useful when you're a heavy window user
and like to split your frames into many windows.

To move 3 windows forward you'd do `C-u 3 C-x o`. To move 2 windows
backwards you'd do `C-u -2 C-x o`.

If you're mostly working with only two windows you probably don't need
to know that much about `other-window` - after all invoking the
command one time will take you to the other window and invoking it a
second time will take you back to the window you were originally
in. I, however, often employ 3 or windows (blessed be the people who
made huge LCD displays so cheap) and I really dislike having to type
`C-u -1 C-x o` (or `C-- C-x o`, `M--1 C-x o` or `M-- C-x o` for that
matter) to go back to the previous window. To alleviate that
particular nuisance I utilize the following little trick:

```cl
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))
```

Now pressing `C-x O` (that's not a zero, it's a capital `o`) will
always take me to the previous window, no matter how many windows are
currently present.

The `C-x O` keybinding is available out-of-the-box in
[Prelude](https://github.com/bbatsov/prelude).
