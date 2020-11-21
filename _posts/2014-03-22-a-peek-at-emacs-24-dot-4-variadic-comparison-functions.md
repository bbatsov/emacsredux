---
layout: post
title: "A peek at Emacs 24.4: Variadic Numeric Comparison Functions"
date: 2014-03-22 16:48
comments: true
tags:
- Emacs 24.4
---

In most Lisps numeric comparison functions like `=`, `<`, `>`, `<=`
and `>=` are variadic - meaning they accept variable number of arguments (as do functions like `+`, `-`, `*`, etc).
Here's an example:

``` elisp
;; let's assume that all those are numeric variables
(= x y z)
(< a b c d)
```

This is pretty cool and save you from writing code like:

``` elisp
(and (= x y) (= y z))
(and (< a b) (< b c) (< c d))
```

Prefix function position for the win!

In Emacs Lisp, however, all these comparison functions (unlike `+`, `-`, etc)
accept just two arguments, which is somewhat unlispy. Luckily this is
one of the things that's going to change with Emacs 24.4.
Emacs Lisp takes another small step to becoming a good Lisp dialect!
