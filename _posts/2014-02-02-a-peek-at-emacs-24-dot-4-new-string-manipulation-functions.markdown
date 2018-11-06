---
layout: post
title: "A peek at Emacs 24.4: New string manipulation functions"
date: 2014-02-02 09:29
comments: true
tags:
- Emacs 24.4
---

Emacs has often been criticized for failing to provide a more
extensive string manipulation API (compared to that of programming
languages like Ruby and Perl, for instance).  As many programs
(extensions) running on top of it are doing quite a lot of string
manipulation, having a good string API is important. To compensate the
lack of certain primitives in Emacs itself a lot of package authors
are using these days packages like
[s.el](https://github.com/magnars/s.el) or simply adding the string
functions they need directly to their packages (to reduce the number
of third-party deps).

In Emacs 24.4 finally the situation is improving. Finally, we're getting
`string-suffix-p`, which was mysteriously missing even though `string-prefix-p`
has been part of Emacs for years:

``` elisp
(string-suffix-p "test" "my_test")
; => t
(string-suffix-p "tester" "my_test")
; => nil
```

More importantly, Emacs 24.4 ships with a new built-in library called `subr-x`, which features
a bunch of other string manipulation functions:

* `string-blank-p`
* `string-empty-p`
* `string-join`
* `string-reverse`
* `string-trim-left`
* `string-trim-right`
* `string-trim`
* `string-remove-prefix`
* `string-remove-suffix`

Here's a brief demo of them in action:

``` elisp
;; all functions in the library are defined as inline, so you don't
;; need to require the library at runtime
(eval-when-compile (require 'subr-x))

(string-empty-p "")
; => t
(string-empty-p "  ")
; => nil
(string-blank-p "  ")
; => 0 (#o0, #x0, ?\C-@)
(string-reverse "Batman")
; => "namtaB"
(string-join '("one" "two" "three"))
; => "onetwothree"
(string-join '("one" "two" "three") ",")
; => "one,two,three"
(string-trim "   Peter Parker ")
; => "Peter Parker"
(string-remove-prefix "Mr. " "Mr. Smith")
; => "Smith"
(string-remove-suffix "Smith" "Mr. Smith")
; => "Mr. "
```

Sure, `subr-x` is not as extensive as `s.el` (and will never be), but
I think that it's a big step in the right direction. It's likely that
`subr-x` will be extended in subsequent Emacs versions and some of the
functions from it will be promoted to `built-in`.

That's all I have for now. Until next time!
