---
layout: post
title: "A peek at Emacs 24.4: prettify-symbols-mode"
date: 2014-08-25 16:30
comments: true
tags:
- Emacs 24.4
---

Emacs 24.4 ships with a new minor mode called
`prettify-symbols-mode`. Its purpose is to replace the standard text
representation of various identifiers/symbols with a (arguably) more
aesthetically pleasing representation (often a single unicode
character would replace several ascii characters).

A classic example would be `lambda` from various Lisp dialects that many people
prefer to replace with the greek letter `λ` (small lambda). `prettify-symbols-mode` allows you
to achieve this by relying on a simple mapping expressed in the form of an `alist` that
each major mode must initialize (`prettify-symbols-alist`).
Simply put - major modes have to provide the configuration for `prettify-symbols-mode`.

Lisp modes do this via `lisp--prettify-symbols-alist`:

``` elisp
(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
```

This means that out of the box only `lambda` will get replaced.
You can, of course, add more mappings for different major modes:

``` elisp
(add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(">=" . ?≥) prettify-symbols-alist)))
```

Let's see the mode in action. Consider this bit of Emacs Lisp code:

``` elisp
(lambda (x y)
  (if (>= x y)
      (something)
      (something-else)))
```

After you do `M-x prettify-symbols-mode` you'll end up with:

``` elisp
(λ (x y)
  (if (≥ x y)
      (something)
    (something-else)))
```

To enable this for a particular mode use `(add-hook 'some-mode-hook 'prettify-symbols-mode)`.
If you'd like to enable it globally just add the following to your config:

``` elisp
(global-prettify-symbols-mode +1)
```

By the way, sky is the limit for symbol prettification. One fairly extreme example would be
vim's plugin [haskell-conceal+](https://github.com/enomsg/vim-haskellConcealPlus) that goes to great
lengths to bring proper mathematical notation to Haskell code. We can achieve more or less the same effect
with `prettify-symbols-mode`, but one have to ask themselves where should we draw the border between
tasteful and distasteful prettifications.
