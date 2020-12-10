---
layout: post
title: "Make use of the Super key"
date: 2013-07-17 16:16
comments: true
tags:
- Keybindings
---

Emacs users have a lot of power at their disposal, but in one
department they are always short - the number of available
non-complex keybindings that they can leverage.

Obviously nobody likes pressing keybindings like `C-c C-v k` (or
something like that). One way to get your hands on some extra
keybindings is to utilize the `Super` key (it's the `Windows` key on
Win keyboards and the `Command` key on Mac keyboards[^1]). One great
thing about `Super` is that you generally have two of them, which
makes them touch-typing friendly. What's more - since almost no packages use those
keys you're left with plenty of options.

[Prelude](https://github.com/bbatsov/prelude) defines a bunch of
global keybindings that use the `Super` key.

``` elisp
;; make some use of the Super key
(define-key global-map [?\s-d] 'projectile-find-dir)
(define-key global-map [?\s-e] 'er/expand-region)
(define-key global-map [?\s-f] 'projectile-find-file)
(define-key global-map [?\s-g] 'projectile-grep)
(define-key global-map [?\s-j] 'prelude-top-join-line)
(define-key global-map [?\s-k] 'prelude-kill-whole-line)
(define-key global-map [?\s-l] 'goto-line)
(define-key global-map [?\s-m] 'magit-status)
(define-key global-map [?\s-o] 'prelude-open-line-above)
(define-key global-map [?\s-w] 'delete-frame)
(define-key global-map [?\s-x] 'exchange-point-and-mark)
(define-key global-map [?\s-p] 'projectile-switch-project)
```

If you find pressing `Super` comfortable obviously you have the
potential to add quite a lot more keybindings to this list.

**P.S.** Some keyboards (notably laptop ones) have a `Fn` key as well
  that's also usable in Emacs keybindings. Somewhat funny that key is
  known in Emacs as `Hyper` (`Star Wars` fans are undoubtedly adding a
  **Hyper-Space** keybinding to their setups right about now).

[^1]: Although many people remap `Command` to `Meta` and `Option` to `Super`.
