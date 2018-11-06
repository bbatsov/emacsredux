---
layout: post
title: "A peek at Emacs 24.4: Focus Hooks"
date: 2014-03-22 16:05
comments: true
tags:
- Emacs24.4
---

A couple of years ago I
[lamented over the lack of frame focus hooks in Emacs](http://batsov.com/articles/2012/03/08/emacs-tip-number-5-save-buffers-automatically-on-buffer-or-window-switch/). This
prohibited us from implemented a feature like IntelliJ IDEA's "auto-save on
focus lost", which was kind of frustrating as we're generally assuming that everything is doable in Emacs!

All this changes in Emacs 24.4 with the introduction of two new hooks - `focus-in-hook` and `focus-out-hook`.
The first is triggered when the current frame gains focus and the second when the frame loses focus.
So, if we want to save our active buffer when the Emacs frame loses focus we can simply do it like this:

``` elisp
(add-hook 'focus-out-hook 'save-buffer)
```

Or you can go a step further and save all the buffers:

``` elisp
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
```

I pretty sure you'll find other creative ways to apply those new hooks to good use!
