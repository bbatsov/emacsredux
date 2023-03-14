---
layout: post
title: Remove Keybinding in Emacs
date: 2023-03-12 10:29 +0200
tags:
- Keybindings
---

Most of the time people ask how to add new keybindings to Emacs and that makes
perfect sense. Occasionally, however, the topic of removing keybindings also emerges - e.g. recently Paredit added a keybinding that messed up some REPL mode that were enabling it. The solution was to remove the problematic keybinding:

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

But wait, there's more! The
[bind-key](https://elpa.gnu.org/packages/bind-key.html) package, that's used
internally by `use-package` and is available on GNU ELPA, can also come in handy
for removing keybindings. It features the macro `unbind-key` that does exactly
what we need. Here's one example usage:

``` emacs-lisp
;; To unbind a key within a keymap (for example, to stop your favorite major
;; mode from changing a binding that you don't want to override everywhere),
;; use `unbind-key':
(unbind-key "C-c x" some-other-mode-map)
```

Note that the terminology when it comes to removing keybinding is pretty messy
as often people use "remove", "unset" or "unbind" interchangeably. That's usually fine, but there's a subtle difference when there's a parent keymap
involved. When unsetting a key in a child map (e.g. with `define-key`), it will
still shadow the same key in the parent keymap. Removing the binding will allow
the key in the parent keymap to be used. Everything in this article except `unbind-key` unsets a keybinding.

One more thing... To overwhelm you even further Emacs 29 introduces `keymap-unset` that can both unset or unbind a keybinding depending on how it's used:

``` emacs-lisp
;; unset a binding
(keymap-unset clojure-mode-map (kbd "C-c C-z"))

;; remove a binding
(keymap-unset clojure-mode-map (kbd "C-c C-z") 'remove)
```

Probably that function will become the golden standard going forward.

That's all I have for you today. Short, sweet and maybe even a bit useful.

**P.S.** Also check out this [follow-up article]({% post_url 2023-03-14-removing-unbinding-vs-unsetting-keybindings %}) that discusses in more detail the topic of "unsetting vs unbinding".
