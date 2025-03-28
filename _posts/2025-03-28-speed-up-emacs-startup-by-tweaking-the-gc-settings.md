---
layout: post
title: Speed up Emacs Startup by Tweaking the GC Settings
date: 2025-03-28 10:06 +0200
tags:
- Startup
---

A well-known Emacs performance optimization advice is to boost the garbage collector
threshold (so GC collections happen less frequently). That's something I've had in
my Emacs config for ages:

``` emacs-lisp
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
```

Probably I should increase it to 100MB+ these days, given the proliferation of more resource-hungry
tooling (e.g. LSP).

One thing that's not so common knowledge is that removing the GC limits during Emacs startup
might improve the speedup quite a lot (the actual results will be highly dependent on your setup).
Here's what you need to do - just add the following bit to your `early-init.el`:

``` emacs-lisp
;; Temporarily increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Restore to normal value after startup (e.g. 50MB)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))
```

`most-positive-fixnum` is a neat constant that represents the biggest positive
integer that Emacs can handle. There's also `most-negative-fixnum` that you might
find handy in some cases.

As for `early-init.el` - it was introduced in version 27 and is executed before
`init.el`. Its primary purpose is to allow users to configure settings that need
to take effect early in the startup process, such as disabling GUI elements or
optimizing performance. This file is loaded before the package system and GUI
initialization, giving it a unique role in customizing Emacs startup behavior.

Here are some other settings that people like to tweak in `early-init.el`:

``` emacs-lisp
;; Disable toolbars, menus, and other visual elements for faster startup:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

;; Load themes early to avoid flickering during startup (you need a built-in theme, though)
(load-theme 'modus-operandi t)

;; tweak native compilation settings
(setq native-comp-speed 2)
```

I hope you get the idea! If you have any other tips on speeding up the Emacs
startup time, I'd love to hear them!

That's all I have for you today. Keep hacking!
