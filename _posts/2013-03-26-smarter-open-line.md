---
layout: post
title: "Smarter open-line"
date: 2013-03-26 15:47
comments: true
tags:
- Editing
---

Often when editing code one wishes to open a line just under the
current one, which is properly indented relative to the existing code,
and position the cursor at its beginning.  Such a feature is present
in most IDEs, such as IntelliJ IDEA, Eclipse and NetBeans. It’s
usually bound to `Shift+Enter`. Emacs has a command `open-line`, which
sadly behave nothing like I'd want it to - it just breaks the current
line and inserts one (or more) empty line afterwards. Luckily in the
land of Emacs any shortfall is correctable with a bit of
Emacs Lisp. Just add this snippet to your `.emacs` (or `.emacs.d/init.el` or
whatever):

``` elisp
(defun er-smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] #'er-smart-open-line)
```

Evaluate the code (or restart Emacs) and you'll be able to use
`M-x smart-open-line` or `Shift+Enter` (a.k.a. `S-return`).

This command is part of
[crux](https://github.com/bbatsov/crux) (it's named
`crux-smart-open-line` there).
