---
layout: post
title: "imenu-anywhere"
date: 2016-01-30 12:45
comments: true
tags:
- imenu
---

For the longest time [Prelude](https://github.com/bbatsov/prelude)
included the function `prelude-goto-symbol` (bound to `C-c i`).
It basically allowed you to jump to any definition in the current source file
using `imenu` behind the curtains.

Recently I've found an even better option - the package
[imenu-anywhere](https://github.com/vspinu/imenu-anywhere). It works in a pretty similar
manner but gives you the ability to jump to any definition in any currently open buffer.
That's quite handy and it greatly reduces the need to use something like `etags`.

As an added bonus - `imenu-anywhere` features helm integration.

This is a very handy package and I encourage you to give it a go!

P.S. Prelude users should simply upgrade to the latest version of
Prelude (it already uses it).
