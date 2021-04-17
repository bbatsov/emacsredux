---
layout: post
title: "My Personal Emacs Configuration"
date: 2016-01-30 12:31
comments: true
tags:
- Configuration
---

From time to time people ask me about my personal Emacs
configuration. Other just assume that I use
[Prelude](https://github.com/bbatsov/prelude). For a very long time my
personal configuration was pretty similar to Prelude - in a way it was
a staging ground for things to go into Prelude eventually (although
changes would travel both ways when Prelude users suggest some cool
things).

Recently I've decided that in the future I want to do a few things with Prelude:

* extract as much functionality from it as possible into reusable
  packages (e.g. [super-save](https://github.com/bbatsov/super-save)
  and [crux](https://github.com/bbatsov/crux))
* adopt there [use-package](https://github.com/jwiegley/use-package)
* improve the support for Windows (because now I have a Windows computer)

As part of these efforts I reworked my personal config into something
pretty simple (it's a single `init.el` file) and I've started
experimenting with ideas for the future. Stay tuned for the results!

The config is available
[here](https://github.com/bbatsov/emacs.d). Perhaps some of you will
find something useful there.
