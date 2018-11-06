---
layout: post
title: "A proper replacement for flet"
date: 2013-09-05 14:38
comments: true
tags:
- utilities
---

The popular `flet` macro was deprecated in Emacs 24.3 and replaced with two similar macros - `cl-flet` and `cl-letf`.

`flet` was used to temporarily override function definitions.
This was an analogue of a dynamically scoped `let` that operates on the function
cell of symbols rather than their value cell.

The ability to dynamically rebind a functions was very useful for stubbing purposes in unit tests (you do write unit tests, don't you?).

``` elisp
(flet ((projectile-project-root () "/path/to/project")
       (projectile-project-name () "project"))
  ...)
```

`projectile-project-root` and `projectile-project-name` are impure
functions (they depend on the current directory) and testing functions
that use them internally would normally be problematic. However, `flet`
gives us the ability to override their actual definitions in our
tests. `flet`'s official replacement `cl-flet` is lexically bound and this is no longer possible
with it.

Fortunately [Nic Ferrier](https://github.com/nicferrier) created a
true drop-in `flet` replacement (with some extra magic baked in) -
[noflet](https://github.com/nicferrier/emacs-noflet). If you're missing `flet`, I suggest you to give `noflet` a try.
