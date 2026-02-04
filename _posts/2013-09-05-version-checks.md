---
layout: post
title: "Version checks"
date: 2013-09-05 14:25
comments: true
tags:
- Emacs Lisp
---

Often the code you're writing would be depending on the version of
some external tool (say `git`) or Emacs itself. Version checks in Emacs are pretty
easy - just use the built-in functions `version=`, `version<=` or `version=`.

Let's illustrate this with an
example. [Prelude](https://github.com/bbatsov/prelude) requires at
least GNU Emacs 24.1 to run and on startup it performs the following
check:

``` elisp
(when (version< emacs-version "24.1")
  (error "Prelude requires at least GNU Emacs 24.1"))
```

Simple and effective. The `version` functions are pretty smart and recognize most popular version formats correctly.
Note that version string "1" is equal to "1.0", "1.0.0", "1.0.0.0",
etc.  That is, the trailing ".0"s are insignificant.  Also, version
string "1" is higher (newer) than "1pre", which is higher than "1beta",
which is higher than "1alpha".  Also, "-CVS" and "-NNN" are treated
as alpha versions.

If you're writing an Emacs package you can also add an explicit Emacs version dependency in the package metadata:

``` elisp
;; Package-Requires: ((emacs "24.1"))
```

That way users of older Emacsen would not see a version of your package targeting newer Emacsen.
