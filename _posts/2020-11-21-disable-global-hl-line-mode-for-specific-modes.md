---
layout: post
title: Disable global-hl-line-mode for Specific Modes
date: 2020-11-21 14:17 +0200
tags:
- Editing
---

A long time ago I suggested the use of [global-hl-line-mode]({% post_url 2013-04-02-highlight-current-line %}) for
highlighting the current line. There's one problem with the mode
I failed to cover back then - namely how to disable it in certain modes
where it doesn't make sense to highlight the current line (e.g. in REPL or terminal buffers).

When recently someone asked me how to disable it in `vterm-mode` I assumed it'd be as easy as:

``` emacs-lisp
;; doesn't work
(add-hook 'vterm-mode-hook (lambda () (hl-line-mode -1)))
```

Unfortunately it turned out that for some reason that doesn't work.
Still, a working solution to our problem is relatively simple:

``` emacs-lisp
(add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
```

This example uses a specific major mode as its target, but you can easily
target a more common parent mode - e.g. most REPL modes in Emacs derive
from `comint-mode`, so you can do something like:

``` emacs-lisp
(add-hook 'comint-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
```

By the way, perhaps it's better not to use the global mode in the first place
and just enable `hl-line-mode` where it makes sense. I think this configuration
should work well for most people:

``` emacs-lisp
;; let's enable it for all programming major modes
(add-hook 'prog-mode-hook #'hl-line-mode)
;; and for all modes derived from text-mode
(add-hook 'text-mode-hook #'hl-line-mode)
```

As the saying goes - less is more.

That's all I have for you today. I hope you learned something useful. Keep hacking!
