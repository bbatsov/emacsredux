---
layout: post
title: "CamelCase aware editing"
date: 2013-04-21 12:52
comments: true
tags:
- Editing
---

One common hurdle Emacs users experience when hacking in Emacs is that
many programming languages use `CamelCase` for class names, method
names, identifiers, etc, but Emacs (unlike most IDEs) doesn't treat
CamelCase words the same special way it treats `lisp-case` and `snake_case`
words. Let me illustrate this with an example.

If you invoke `kill-word`(`M-d`) when your cursor is before
`lisp-case` this would kill just `lisp` and leave you with `-case`. If
you do the same with `CamelCase` the entire "word" CamelCase will be
killed. Same goes for navigation-by-word commands like `forward-word`,
`backward-word`, etc - they are aware of the subwords that comprise a
`lisp-case` or a `snake_case` word, but are oblivious to
`CamelCase`. So, what can we do? The answer is surprisingly simple -
just use the built-in minor mode `subword-mode` (formerly known as
`c-subword-mode`).

``` elisp
;; enable just in ruby-mode
(add-hook 'ruby-mode-hook 'subword-mode)

;; enable for all programming modes
(add-hook 'prog-mode-hook 'subword-mode)
```

At this point you can try again `kill-word` before `CamelCase`. Only
`Camel` is killed now. Sweet!

[Prelude](https://github.com/bbatsov/prelude) enables `subword-mode`
out of the box for programming modes that make use of `CamelCase`.
