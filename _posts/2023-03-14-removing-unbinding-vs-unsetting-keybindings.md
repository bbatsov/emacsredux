---
layout: post
title: Removing (Unbinding) vs Unsetting Keybindings
date: 2023-03-14 12:09 +0200
tags:
- Keybindings
---

Recently I wrote a short article on [removing keybindings]({% post_url 2023-03-12-remove-keybinding-in-emacs %}). Originally
I failed to cover there one important nuance there that I'll tackle today - namely
the difference between really removing (unbinding) a keybinding versus just
unsetting it (setting it to `nil`).[^1] I know what you're thinking right now - is
there really any difference between the two?

Most of the time there's no real difference and I guess that's why people often
use the terms "remove", "unbind" and "unset" interchangeably. But they are not
the same as there's a subtle difference when there's a parent keymap
involved. When unsetting a key in a child map (e.g. with `define-key`), it will
still shadow the same key in the parent keymap. Removing the binding will allow
the key in the parent keymap to be used. That's why one can argue that unbinding
is preferable to unsetting. The only problem with unbinding is that it was kind
of hard to do in Emacs until very recently, unless you were relying on third-party
packages to do so (e.g. `bind-key`).

Generally the best way to truly remove a keybinding is probably the new command
`keymap-unset` that will be part of Emacs 29. It can both unset and unbind a
keybinding depending on how it's used:

``` emacs-lisp
;; unset a binding
(keymap-unset clojure-mode-map (kbd "C-c C-z"))

;; remove a binding
(keymap-unset clojure-mode-map (kbd "C-c C-z") 'remove)
```

By the way, you can use the `keymap-unset` even on older Emacs version if you install the [compat package](https://elpa.gnu.org/packages/compat.html), that brings newer Emacs functionality (mostly newer APIs) to older Emacs releases.

I hope this article cleared some of the confusion with all the overloaded terminology in the field. That's all I have for you today. Keep hacking!

[^1]: Special thanks to [Jonas Bernoulli](https://www.reddit.com/r/emacs/comments/11qblyt/comment/jc4158f/?utm_source=share&utm_medium=web2x&context=3) for flagging my omission.
