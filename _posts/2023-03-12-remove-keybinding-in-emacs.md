---
layout: post
title: Remove Keybinding in Emacs
date: 2023-03-12 10:29 +0200
tags:
- Keybindings
---

Most of the time people ask how to add new keybindings to Emacs and that makes
perfect sense. Occasionally, however, the topic of removing keybindings also emerges - e.g. recently Paredit added a keybinding that messed up some REPL mode that were enabling it. The solution was to remove (unset) the problematic keybinding:

``` emacs-lisp
(define-key paredit-mode-map (kbd "RET") nil)
```

Basically to remove a keybinding you just have to set it to `nil`. This works
both for mode-specific keybindings (as demonstrated above) and for global/local
keybindings:

``` emacs-lisp
(global-set-key (kbd "C-e") nil)

;; local keybindings are specific only to the current buffer
(local-set-key (kbd "C-e") nil)
```

You can also use commands like `global-unset-key` and `local-unset-key`, e.g. like this:

``` emacs-lisp
(global-unset-key (kbd "C-e"))

;; local keybindings are specific only to the current buffer
(local-unset-key (kbd "C-e"))
```

As a bonus, `global-unset-key` and `local-unset-key` can also be used interactively (with `M-x`).

That's all I have for you today. Short, sweet and maybe even a bit useful.
