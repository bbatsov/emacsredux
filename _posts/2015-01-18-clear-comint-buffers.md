---
layout: post
title: "Clear Comint Buffers"
date: 2015-01-18 21:57
comments: true
tags:
- Utilities
- comint
---

`comint` provides Emacs infrastructure for building command
interpreters. It's backing a lot of popular shell/REPL modes - like `eshell`,
`inferior-lisp`, `inf-ruby`, `inf-clojure`, etc.

`comint` provides a myriad of built-in commands, but somewhat surprisingly
it doesn't feature a command to clear the contents of a `comint` buffer.
Let's write one such command ourselves!

While there are several way to tackle it, I feel this is the simplest
(and the one that makes the best use of `comint`'s existing
functionality):

``` elisp
(defun comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; let's bind the new command to a keycombo
(define-key comint-mode-map "\C-c\M-o" #'comint-clear-buffer)
```

Simple and elegant, right? One day it might even end up being part of `comint` itself.
