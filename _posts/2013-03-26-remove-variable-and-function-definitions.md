---
layout: post
title: "Remove Variable &amp; Function Definitions"
date: 2013-03-26 17:15
comments: true
tags:
- Emacs Lisp
---

From time to time you might want to void (unbind) a variable or a
function definition in Emacs. Most often you'll probably be dealing
with variables created with `defvar`, whose values you'll want to
update.  The magic functions you need are the following:

``` elisp
;; this will make the symbol my-nasty-variable's value void
(makunbound 'my-nasty-variable)
;; this will make the symbol my-nasty-function's
;; function definition void
(fmakunbound 'my-nasty-function)
```

The names aren't exactly intuitive and even I happen to forget them from time to
time.
