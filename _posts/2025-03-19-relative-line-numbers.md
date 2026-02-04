---
layout: post
title: Relative Line Numbers
date: 2025-03-19 13:49 +0200
tags:
- Configuration
- Built-ins
---

Relative line numbers (relative to the current line) are super popular in the world of Vim,
because there it's super easy to move `n` lines up or down with `j` and `k`.
In the world of Emacs most of us tend to just go some line using `M-g g` using a absolute
line number or using `avy` (`avy-goto-line`).

That being said, relative line numbers are easy to enable in Emacs and quite handy if you're into
`evil-mode`:

``` emacs-lisp
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)
```

Relative line numbers are useful with the Emacs core commands `forward-line`
(`C-n`) and `previous-line` (`C-p`) as well.  Just trigger them with the universal prefix
`C-u` and you can move quickly around:

- `C-u 5 C-n` (move 5 lines forward)
- `C-u 10 C-p` (move 10 lines backward)

Easy-peasy!

That's all I have for you today! Keep hacking!
