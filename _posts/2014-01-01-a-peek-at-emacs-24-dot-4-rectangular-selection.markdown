---
layout: post
title: "A peek at Emacs 24.4: Rectangular selection"
date: 2014-01-01 12:03
comments: true
tags:
- Emacs 24.4
---

I'm extremely fond of rectangle editing (the ability to select a
rectangular region and apply editing commands only to it), but I've
always disliked the fact there was no way to visually highlight the
exact rectangle region you've currently selected. Let's delete some
text in a rectangular region (with `C-x r d`), so you can see where the
problem lies:

![old rectangular selection](/assets/images/rect-old.gif)

It's not immediately clear where the rectangle bounds are (it's clear
where the rectangle region starts, but it's not so clear where it
ends), which is a source of constant confusion for many people.

Emacs 24.4 finally fixed this with the introduction of
`rectangle-mark-mode` (bound by default to `C-x SPC`). Let's see it in
action:

![new rectangular selection](/assets/images/rect-new.gif)

Perfect!
