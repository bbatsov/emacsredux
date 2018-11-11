---
layout: post
title: "MELPA Stable"
date: 2014-05-16 14:52
comments: true
tags:
- Package Management
- MELPA
---

We all know and love the
[MELPA package repository](http://melpa.milkbox.net/#/). At least we
all know it, but not everyone loves the idea of using package
snapshots.  Some people would rather use only _stable_ package
releases and that's understandable. Unfortunately the major community
repository for stable packages [Marmalade](http://marmalade-repo.org/)
is often plagued by technical issues (that should be fixed once its
`elnode`-powered rewrite is ready) and stale packages (due to the fact
the sometimes people upload packages they don't own there and stop
updating them after a while).

Now there's an alternative to Marmalade, which offers a MELPA-like
experience - [MELPA stable](http://melpa-stable.milkbox.net/#/). It
offers stable packages built automatically from `git` tags, meaning
the only thing you have to do to release a new version of a package
(assuming there's a MELPA recipe for it, of course) is to tag the release in
`git`. No more stale packages! No more manual package uploads!


To use the new repo add this to your Emacs config:

``` elisp
(add-to-list 'package-archives
'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
```

Currently more than 500 packages are available for installation there.
That number is bound to grow (a lot).

By the way, the maintainers of `MELPA` are looking for help to update
the home page of the stable package archive.  Currently it's the same
as the one of the snapshot archive, which is a bit confusing
(especially regarding installation instructions). If you'd like to
help fork and improve the
[page's code on GitHub](https://github.com/milkypostman/melpa/tree/master/html).
