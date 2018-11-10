---
layout: post
title: "Converting between symbols and strings"
date: 2014-12-05 17:28
comments: true
tags:
- Emacs Lisp
---

Sometimes you might need to convert a symbol to string (or vice versa) in Emacs Lisp. You start
looking for functions like `symbol-to-string` and `string-to-symbol` but, alas, they do not seem exist.
Do not despair! Such functions do actually exist, although their names are likely to surprise you:

``` elisp
(symbol-name 'some-symbol)
; => "some-symbol"
(intern "some-symbol")
; => some-symbol
```
