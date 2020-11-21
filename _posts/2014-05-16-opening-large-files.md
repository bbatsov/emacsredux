---
layout: post
title: "Opening Large Files"
date: 2014-05-16 12:37
comments: true
tags:
- config
---

I guess everyone has seen the following warning (at some point or
another) when trying to open a relatively large file in Emacs:

```
File foo.bar is large (XX.XM), really open?
```

By default Emacs will display this warning for every file bigger than
10MB, which is quite annoying (conservative), especially given the
fact that on any modern computer even much bigger files will be loaded
almost instantaneously and won't slow down Emacs significantly.

You can adjust this behavior by altering the value of the
`large-file-warning-threshold` variable. I'd suggest setting it to
something like 100MB:

``` elisp
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
```

Alternative, you can set it to `nil`, which will suppress the
warning permanently, regardless of the size of the file you're trying to open.

**P.S.** By the way, it's not particularly wise to edit **huge** files
(say bigger than 1GB) in Emacs directly.  If you need to do something
like this I'd suggest taking a look at the excellent
[vlfi](https://github.com/m00natic/vlfi) package.
