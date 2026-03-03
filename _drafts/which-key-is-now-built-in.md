---
layout: post
title: which-key is Now Built-in
date: 2026-02-28 09:00 +0200
tags:
- Emacs 30
- Built-ins
---

If you've been using Emacs for a while, chances are you've come across the
popular `which-key` package. It's one of those packages that practically everyone
recommends to newcomers (and power users alike) — and for good reason. When you
start a key sequence like `C-x` and pause for a moment, `which-key` pops up a
helpful buffer showing all available completions. It's like having a cheat sheet
that appears exactly when you need it.

Starting with Emacs 30, `which-key` is now a **built-in package**. No more
fetching it from MELPA — it ships with Emacs out of the box.

## Enabling which-key

Despite being built-in, `which-key-mode` is not enabled by default. You'll need
to turn it on explicitly:

```emacs-lisp
(which-key-mode 1)
```

Once enabled, just type a prefix key (e.g. `C-x`, `C-c`, `C-h`) and wait for a
second. A popup will appear in the minibuffer showing all available key bindings
that follow.

## Configuration

The defaults are reasonable, but here are the knobs you're most likely to want to tweak:

```emacs-lisp
;; Show the popup faster (default is 1 second)
(setq which-key-idle-delay 0.5)

;; Change where the popup appears
;; Options: 'minibuffer, 'side-window, 'frame, 'custom
(setq which-key-popup-type 'side-window)

;; Control the side window position
;; Options: 'bottom, 'top, 'left, 'right
(setq which-key-side-window-location 'bottom)
```

## But wait, what about C-h?

Long-time Emacs users might point out that you've always been able to press `C-h`
after a prefix key to see available bindings. That's true! The difference is that
`which-key` does this *automatically* — no extra keypress needed. It's a more
discoverable, more beginner-friendly approach.

Of course, `C-h` after a prefix still works and gives you a more traditional
`describe-bindings` style buffer. The two approaches complement each other
nicely.

That's all I have for you today. Keep hacking!
