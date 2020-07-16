---
layout: post
title: "Delete Whitespace around Point"
date: 2013-05-19 12:08
comments: true
tags:
- Editing
---

One task that often pops when editing text or code is the need to
remove some extra whitespace around the point (the cursor). Emacs has your back!

First, we have the command `delete-horizontal-space` (bound to `M-\`) -
it deletes all spaces and tabs around the point. With a prefix
argument (`C-u M-\`) the command deletes only the whitespace before the point.

One similar, although a bit less known, command is
`just-one-space` (bound to `M-SPC`) - it deletes all spaces and tabs
around the point, leaving just one space (or `N` spaces if you supply `N` as a
prefix argument like `C-4 M-SPC`). If `N` is negative, the command
deletes newlines as well, leaving `-N` spaces.

One minor snafu with `just-one-space` is that `M-SPC` is a no-go on
OS X since just about everyone uses it for quick access to Spotlight or
switching between languages. You might want to use a different
keybinding like this one:

``` elisp
(global-set-key (kbd "C-c j") 'just-one-space)
```

Personally, I find `just-one-space` to be the more useful command of the two. You can
potentially consider remapping `delete-horizontal-space` to `just-one-space`,
due to it's convenient keybinding[^1]:

``` elisp
(global-set-key [remap delete-horizontal-space] 'just-one-space)
```

In [Prelude](https://github.com/bbatsov/prelude) `just-one-space` is
additionally bound to the `kk`
[keychord](/blog/2013/04/28/execute-commands-ninja-style-with-key-chord-mode/).

[^1]: At least for OS X users. Obviously in general `M-SPC` is easier to press.
