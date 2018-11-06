---
layout: post
title: "Playing with Font Sizes"
date: 2013-04-01 14:22
comments: true
tags:
- Utilities
---

Adjusting interactively the font size is Emacs is pretty easy. Three
keybindings refer to the mighty `text-scale-adjust` command - `C-x
C-+`(increase font size), `C-x C--`(decrease font size) and `C-x
C-0`(reset font size to default). After using any of the keybindings
you can proceed to adjust the font size further just by pressing `+`,
`-` or `0` respectively.  `text-scale-adjust` is mostly a wrapper
around `text-scale-increase` and `text-scale-decrease`.

You might want to bind them `C-+` and `C--`(generally a bad idea since
you won't be able to type stuff like `C-- some-command` anymore) to
get more common keybindings for the operations.

``` elisp
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
```
