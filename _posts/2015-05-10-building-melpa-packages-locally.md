---
layout: post
title: "Building MELPA packages locally"
date: 2015-05-10 11:00
comments: true
tags:
- melpa
- package.el
---

Don't want to wait for the MELPA packages to be automatically rebuilt?
Want to try out some package recipe before submitting it upstream? You can
totally do this!

Just clone the
[MELPA github repo](https://github.com/milkypostman/melpa), open a
recipe from it in Emacs and press `C-c C-c` (bound to the command
`package-build-current-recipe`). Once the package is built you'll be
prompted to install it.

Pretty neat, right?
