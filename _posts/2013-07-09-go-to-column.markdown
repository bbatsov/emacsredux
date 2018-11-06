---
layout: post
title: "Go To Column"
date: 2013-07-09 16:30
comments: true
tags:
- Utilities
---

Almost every Emacs user knows that `M-g M-g` and `M-g g` (both bound to
`go-to-line`) will take him to the line of his choosing (provided he
knows the number of the target line, of course).

Surprisingly few Emacs users know that there is a similar way to jump
to a column by its number - `M-g TAB` (bound to
`move-to-column`). Interactively you cannot jump past the end of the
line you're currently on, but you can always cook your own version of
the command to get around that limitation:

``` elisp
(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))
```

Let's bind that to some keycombo:

``` elisp
(global-set-key (kbd "M-g M-c") 'go-to-column)
```
