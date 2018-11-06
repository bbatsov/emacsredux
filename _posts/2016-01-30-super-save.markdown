---
layout: post
title: "super-save"
date: 2016-01-30 11:26
comments: true
tags:
- misc
---

A while back I wrote
[an article on saving buffers when they lose focus](http://batsov.com/articles/2012/03/08/emacs-tip-number-5-save-buffers-automatically-on-buffer-or-window-switch/).

Recently I've packaged (an improved version of) this functionality
into a tiny global minor mode called
[super-save](https://github.com/bbatsov/super-save).

The package is available on MELPA and MELPA Stable and enabling it is trivial:

``` elisp
(super-save-mode +1)
```

If you want to enable the additional feature of auto-saving buffers
when Emacs is idle, add the following as well:

``` elisp
(setq super-save-auto-save-when-idle t)
```

If you're like me and don't care about the backups created by the
built-in `auto-save-mode`, you can disable it aftewards:

``` elisp
(setq auto-save-default nil)
```

I've been using Emacs for over 10 years
now and I've never needed the auto-created backups - I'm either very
lucky or this is less useful than it's supposed to be.
