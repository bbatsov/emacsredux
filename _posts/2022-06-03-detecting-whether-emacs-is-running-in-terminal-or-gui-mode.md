---
layout: post
title: Detecting Whether Emacs is Running in Terminal or GUI mode
date: 2022-06-03 10:25 +0300
tags:
- Configuration
---

Occasionally we'd have bits of configuration that are depending on whether
Emacs is running in a terminal or in GUI mode. This article aims to cover
how to best handle such situations.[^1]

At first it seems the solution is super simple:

``` emacs-lisp
(if (display-graphic-p)
    ;; GUI mode
    (progn
      (your)
      (code))
    ;; Terminal mode
    (your)
    (code))
```

In the past the variable `window-system` provided another way to check for this, but it has been deprecated for a while, so I'd advise against using it.

While the above solution works, it fails to address one fundamental aspect of Emacs - you can have multiple Emacs frames (or windows, in non-Emacs terminology) associated with one Emacs instance. Some of those might be on a terminal, and others might be on a window system. That is to say, you can get different values of `display-graphics-p` even within a single Emacs instance.

For example, you can start a GUI Emacs and then connect to it via `emacsclient -t` in a terminal; the resulting terminal frame will see a value of `nil` for `display-graphics-p`. Similarly, you can start Emacs in daemon mode (`emacs --daemon`), then later tell it to create a graphical frame with `emacsclient -c`.

What this means in practice is that it's usually best to put code that's GUI/terminal specific in `after-make-frame-functions`:

``` emacs-lisp
(add-hook 'after-make-frame-functions
  (lambda ()
    ;; we want some font only in GUI Emacs
    (when (display-graphics-p)
      (set-frame-font "DejaVu Sans Mono 28")))
```

or

``` emacs-lisp
(add-hook 'after-make-frame-functions
  (lambda ()
    ;; we do something only in terminal Emacs
    (unless (display-graphics-p)
      (xterm-mouse-mode 1)))
```

That way the check would happen separately for each frame and you'd be able to adjust the settings for it accordingly. And yeah - probably enabling a global mode like `xterm-mouse-mode` is not the best example for a frame-specific action to take, but I hope you get the general idea.

Note that the `after-make-frame-functions` hook isn't run for the initial frame, so it's often necessary to also add frame-related hook functions like that above to `after-init-hook`.

That's all I have for you today. Keep hacking!

[^1]: The article is inspired by [this StackOverflow discussion](https://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode).
