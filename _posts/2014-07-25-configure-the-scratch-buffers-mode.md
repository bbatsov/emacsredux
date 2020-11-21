---
layout: post
title: "Configure the Scratch Buffer's Mode"
date: 2014-07-25 15:23
comments: true
tags:
- Configuration
---

Pretty much every Emacs user knows of the `*scratch*` buffer - after
all it's always present when you start your Emacs. If you've set
`inhibit-startup-screen` to `t` (which I recommend) it will even be
the very first buffer see each time you fire up your beloved editor
(unless you alter `initial-buffer-choice`, that is).

The scratch buffer was conceived a scratchpad for notes and Emacs Lisp
experiments and a lot of Emacs users find it quite handy.  On the
other hand - even more users don't really write Emacs Lisp and
probably find little value in this buffer.

One little know fact about the `*scratch*` buffer is that its initial
major mode (by default it's `lisp-interaction-mode`) is not hardcoded
and you can alter it.  If you're a Ruby developer you might opt to
make it use `ruby-mode` by default:

``` elisp
(setq initial-major-mode 'ruby-mode)
```

If you do this I'd also advise you to change the buffer's initial
contents. By default they are:

``` elisp
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
```

For Ruby something like this might be better:

``` elisp
(setq initial-scratch-message "\
# This buffer is for notes you don't want to save, and for Ruby code.
# If you want to create a file, visit that file with C-x C-f,
# then enter the text in that file's own buffer.")
```

Or you can simply set `initial-scratch-message` to `nil` and be done with it.
