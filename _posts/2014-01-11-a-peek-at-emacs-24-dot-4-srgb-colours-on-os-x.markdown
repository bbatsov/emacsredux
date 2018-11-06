---
layout: post
title: "A peek at Emacs 24.4: sRGB colours on OS X"
date: 2014-01-11 13:40
comments: true
tags:
- osx
- Emacs24.4
---

A while ago I wrote about a
[colour theme problem specific to OS X](http://emacsredux.com/blog/2013/08/21/color-themes-redux/).
Thankfully in Emacs 24.4 the OS X (a.k.a. NS) port of Emacs uses sRGB
colours by default (at least on OS X Lion (10.7) and newer), so you'll
no longer need to install Emacs from `homebrew` (or apply an sRGB
colour support patch manually). If for some reason you want to stick
to the colours add this to your Emacs config:

``` elisp
(setq ns-use-srgb-colorspace nil)
```

I doubt that anyone would want to do that, but the option is there for those of you who want it.
