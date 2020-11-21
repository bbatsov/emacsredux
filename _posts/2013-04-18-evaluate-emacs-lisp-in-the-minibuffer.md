---
layout: post
title: "Evaluate Emacs Lisp in the Minibuffer"
date: 2013-04-18 14:44
comments: true
tags:
- Utilities
- Emacs Lisp
- Paredit
---

The Emacs Lisp interpreter's integration with Emacs is pervasive. One
can evaluate Emacs Lisp just about everywhere in so many different
ways. One way I find particularly handy (especially for short snippets of
code) is evaluating Emacs Lisp directly in the minibuffer. This is
done with `M-:` (bound to the command `eval-expression`).

Upon typing this magic keycombo you'll be presented with the prompt
`Eval: ` in the minibuffer and there you can enter any Lisp
expression. Hitting `Return` will result in the expression's
evaluation.

There is one minor snafu with this, though - I'm addicted to
[Paredit](http://emacsrocks.com/e14.html) (for editing Lisp code at
least) and Paredit is not automatically enabled in the
minibuffer. Let's change that:

``` elisp
(defun er-conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'er-conditionally-enable-paredit-mode)
```

Evaluate this snippet and try `M-:` again. If you're like me - the
results will please you.

It should come as no surprise that
[Prelude](https://github.com/bbatsov/prelude) enables Paredit by
default for `eval-expression`.
