---
layout: post
title: "A peek at Emacs 24.4: hash-table-keys &amp; hash-table-values"
date: 2014-02-02 09:59
comments: true
tags:
- Emacs 24.4
---

While Emacs has a pretty good hash-table API, two functions were
rather mysteriously absent from it - `hash-table-keys` and
`hash-table-values` (which would return a list of all keys/values in a
hash-table).

Many people who needed them simply defined them in their code
directly (usually in terms of `maphash`), but that's no longer
necessary in Emacs 24.4, as they are now part of the new built-in
library `subr-x`.

``` elisp
;; all functions in the library are defined as inline, so you don't
;; need to require the library at runtime
(eval-when-compile (require 'subr-x))

(setq h (make-hash-table))

(puthash "Batman" "Bruce Wayne" h)
(puthash "Spiderman" "Peter Parker" h)
(puthash "Superman" "Clark Kent" h)

(hash-table-keys h)
; => ("Batman" "Spiderman" "Superman")

(hash-table-values h)
; => ("Bruce Wayne" "Peter Parker" "Clark Kent")
```

Nice and simple.
