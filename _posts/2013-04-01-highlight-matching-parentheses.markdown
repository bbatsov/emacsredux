---
layout: post
title: "Highlight matching parentheses"
date: 2013-04-01 14:40
comments: true
tags:
- Editing
---

If you're into programming highlighting the matching parentheses in
your code is probably something you find rather desirable. Emacs
doesn't do so by default, but the global minor mode `show-paren-mode`,
that comes bundled with Emacs, does.

``` elisp
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)
```

Valid styles are `parenthesis` (meaning highlight the matching
parentheses), `expression` (meaning show the entire expression
enclosed by the parentheses) and `mixed` (meaning show the matching
parentheses if both are visible, and the expression otherwise).

Personally I find `parenthesis` as the least distracting option and
therefore I favor it.

If you'd like to change the faces used for matching and mismatched
parentheses you'd have to alter `show-paren-mismatch` and
`show-paren-match`. Most decent color themes do this out the box.

`show-paren-mode` is enabled out-of-the-box in
[Prelude](https://github.com/bbatsov/prelude).
