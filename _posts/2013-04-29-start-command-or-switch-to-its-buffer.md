---
layout: post
title: "Start command or switch to its buffer"
date: 2013-04-29 17:14
comments: true
tags:
- Utilities
---

Few weeks ago I showed you a
[handy way to run `term-mode`](/blog/2013/03/29/terminal-at-your-fingertips/). You
might have noticed that it makes sense for many commands to be run in
a similar manner. Here's a quick example - I often like to jump
between an Emacs Lisp source buffer and an `ielm` (interactive Emacs
Lisp shell - `M-x ielm`) buffer to try out stuff. I could reuse the
code I showed you for `ansi-term` to create a similar command called `visit-ielm`:

``` elisp
(defun visit-ielm ()
  "Create or visit a `ielm' buffer."
  (interactive)
  (if (not (get-buffer "*ielm*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ielm))
    (switch-to-buffer-other-window "*ielm*")))
```

You might want to bind this to `C-c C-z` (a-la SLIME):

``` elisp
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'visit-ielm)
```

I don't know about you, but I hate code repetition. Having that in
mind we can factor out the duplication like this:

``` elisp
(defun start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (start-or-switch-to (lambda ()
                         (ansi-term (getenv "SHELL")))
                      "*ansi-term*"))

(defun visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (prelude-start-or-switch-to 'ielm "*ielm*"))
```

Much better! We can now use `start-or-switch-to` to build any number of similar commands!

`start-or-switch-to` and `visit-ielm` are available in
[Prelude](https://github.com/bbatsov/prelude)(but with a `prelude-`
prefix).

**P.S.** If you'd like some nice SLIME-like code navigation command in
`emacs-lisp-mode` you might check out
[elisp-slime-nav](https://github.com/purcell/elisp-slime-nav).

SLIME allows very convenient navigation to the symbol at point (using
`M-.`), and the ability to pop back to previous marks (using `M-,`).

This plugin provides similar navigation for Emacs Lisp, supporting
navigation to the definitions of variables, functions, libraries and
faces.

Additionally, `elisp-slime-nav` provides a way to describe the symbol at
point, whatever its type. As with `slime-describe-symbol`, this
functionality is bound both to `C-c C-d d` and `C-c C-d C-d` by default.

It's pretty useful if you hack Emacs Lisp yourselves. If you don't
hack Emacs Lisp (yet) you probably can do without it.

`elisp-slime-nav` comes bundled with Prelude.
