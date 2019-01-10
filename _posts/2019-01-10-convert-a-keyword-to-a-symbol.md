---
layout: post
title: Convert a Keyword to a Symbol
date: 2019-01-10 09:48 +0200
tags:
- Emacs Lisp
---

Recently I needed to convert keywords into symbols in Emacs Lisp and I
noticed there was no built-in function for this, so I've decided to
build one myself. Here's my approach:

``` emacs-lisp
(defun keyword-to-symbol (keyword)
  "Convert KEYWORD to symbol."
  (intern (substring (symbol-name keyword) 1)))

(keyword-to-symbol :foo) ; => 'foo
```

It's extremely simple really - using the fact that keywords in Emacs
Lisp are actually symbols we can convert them to a string with
`symbol-name`, drop their leading `:` and then convert them back to
symbols.

For me the most interesting thing here was learning that keywords are symbols:

``` emacs-lisp
(symbolp :foo) ; => t
```

This was certainly a surprise the first time I saw it, although it
does make sense.  That prompted me to check how are keywords
differentiated from symbols and I found the answer in the
documentation of `keywordp`:

```
keywordp is a built-in function in ‘C source code’.

(keywordp OBJECT)

Return t if OBJECT is a keyword.
This means that it is a symbol with a print name beginning with ‘:’
interned in the initial obarray.
```

While I doubt that you'll often (ever?) have to deal with keyword to
symbol conversion I hope you've learned something fun and useful
today! Keep hacking!
