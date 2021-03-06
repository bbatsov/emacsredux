---
layout: post
title: "Instant Access to init.el"
date: 2013-05-18 19:00
comments: true
tags:
- Utilities
- crux
---

One of the files Emacs users often edit is `init.el`(especially if they
are having all of their configuration inside of it). It might be
sensible then to develop a faster way to access `init.el`:

``` elisp
(defun er-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
```

`user-init-file` holds the path to your `init.el`. For mine its value
is `/Users/bozhidar/.emacs.d/init.el`. `find-file-other-window` will
open the file in a window adjacent to the one you're currently in.

Short and sweet! You might want to bind it to some keycombo:

``` elisp
(global-set-key (kbd "C-c I") #'find-user-init-file)
```

Thanks to [Sebastian Wiesner](https://github.com/lunaryorn) for
bringing this tip to my attention!

This command is available in [crux](https://github.com/bbatsov/crux) as
`crux-find-user-init-file`. This command is also available in
[prelude](https://github.com/bbatsov/prelude) via the crux package.
