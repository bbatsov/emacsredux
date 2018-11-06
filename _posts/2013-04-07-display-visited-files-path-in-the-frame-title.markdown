---
layout: post
title: "Display visited file's path in the frame title"
date: 2013-04-07 14:19
comments: true
tags:
- UI
---

I'm fond of seeing somewhere the full path to the file I'm currently
editing(as opposed to just the file name displayed in
modeline). Emacs's frame title seems like a good place to display such
information, since by default it doesn't show anything
interesting. Here's how we can achieve this:

``` elisp
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
```

The path will be displayed in an abbreviated manner(`/home/bozhidar/`
will be shortened to just `~/`). If you'd like to see the expanded path
just remove the `abbreviate-file-name` function invocation. If a
buffer is not visiting a file, the buffer's name would be displayed
instead.

[Prelude](https://github.com/bbatsov/prelude) sets
`frame-title-format` like this by default.
