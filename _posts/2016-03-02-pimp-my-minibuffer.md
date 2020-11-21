---
layout: post
title: "Pimp My Minibuffer Evaluation"
date: 2016-03-02 18:57
comments: true
tags:
- minibuffer
---

In Emacs you can evaluate Emacs Lisp pretty much anywhere - even
[in the minibuffer]({% post_url 2013-04-18-evaluate-emacs-lisp-in-the-minibuffer %}). Writing
Emacs Lisp in the minibuffer, however, is not exactly fantastic
experience out-of-the-box - there's `TAB` completion, but what about
`eldoc` and `paredit` for instance?

If only there was a way to enable them... I suggested one trick in my
original post on `eval-expression`, but Emacs 24.4 made things even
easier by adding `eval-expression-minibuffer-setup-hook`. To enable
`eldoc` for minibuffer evaluations use this snippet:

``` elisp
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
```

For `paredit` you can use this one:

``` elisp
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
```

Obviously you can do the same for any other minor mode you might need.

The best thing about this setup is that it will work with tools like
[CIDER](https://github.com/clojure-emacs/cider) and
[SLIME](https://github.com/slime/slime) as well (they have similar
commands which allow you to evaluate Clojure & Common Lisp code and
those command trigger `eval-expression-minibuffer-setup-hook`).
