---
layout: post
title: "Eval and Replace"
date: 2013-06-21 12:35
comments: true
tags:
- Utilities
---

Sometimes people tend to overlook how well Emacs and Emacs Lisp are
integrated. Basically there is no limit to the places where you can
evaluate a bit of Emacs Lisp and reap the associated benefits. From
time to time I find myself editing something and thinking - "Hey, it'd
be really great of I could just insert the result of some Emacs Lisp
expression at point!" (my thoughts are pretty crazy, right?). Here's a
contrived example - I might have to enter somewhere the result of
`1984 / 16`. I can calculate that manually or I can fire up `M-x calc`
and get the result, or I can play extra smart and devise the following
command (which I did not actually devise - I'm pretty sure I saw it
in someone else's config a while back):

``` elisp
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
```

Let's bind that to `C-c e`:

``` elisp
(global-set-key (kbd "C-c e") 'eval-and-replace)
```

Now in the buffer I'm currently editing I can type `(/ 1984 16)` and
press `C-c e` afterwards getting the result `124` replace the original
expression. Pretty neat!

I'll leave it up to you to think of more creative applications of the command.

This command is part of
[Prelude](https://github.com/bbatsov/prelude)(it's named
`prelude-eval-and-replace` there).
