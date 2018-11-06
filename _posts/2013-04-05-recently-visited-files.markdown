---
layout: post
title: "Recently visited files"
date: 2013-04-05 14:50
comments: true
tags:
- Utilities
---

Emacs does not keep track of recently visited files by default. Sad,
but true.  On a more positive note - it has the feature(courtesy of
`recentf`), but it's simply not enabled out-of-the-box. Let's see what
we can do to change that:

``` elisp
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)
```

That wasn't that hard. Now Emacs will keep track of the last `200`
files we visited and will show the last 15 of them in the `File->Open
recent`(this menu entry is added by `recentf`) menu. That's it.

At this point you're probably feeling let down. After all - who uses
the menu bar in Emacs anyways? Fortunately there is also a command
named `recentf-open-files`.  Upon invoking it with `M-x
recentf-open-files` you'll be presented with a list of all recently
visited files.

If you're an `ido` user you might prefer to use some command based on
it instead. Here's one:

``` elisp
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
```

I'd suggest binding whichever command you prefer to either `C-c f` or
`C-x C-r` (bound by default to the infrequently used
`find-file-read-only` command):

``` elisp
(global-set-key (kbd "C-c f") 'recentf-ido-find-file)
```

As usual - both the command `recentf-ido-find-file` and its keybinding
`C-c f` are available in
[Prelude](https://github.com/bbatsov/prelude)(obviously `recentf` is
enabled there out-of-the-box).
