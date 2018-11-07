---
layout: post
title: "A peek at Emacs 24.4: superword-mode"
date: 2014-08-27 18:24
comments: true
tags:
- Emacs24.4
---

In a previous post I wrote about
[camel-case aware editing with subword-mode]({% post_url 2013-04-21-camelcase-aware-editing %}). Emacs
24.4 adds a complementary minor mode called `superword-mode`, which
also alters the behavior of word-based commands when enabled.

Normally Emacs would consider underscores and dashes word separators
(`snake_case` and `lisp-case` anyone?). This affects all `word`
commands - `forward-word`, `backward-word`, `kill-word`, etc. Let's
see a couple of examples (`|` denotes the cursor position):

```
;; word with dash
|some-word

;; press M-f (forward-word) once
some|-word

;; press M-f again
some-word|

;; press M-b (backward-word) once
some-|word

;; word with underscore
|some_word

;; press M-f once
some|_word

;; press M-f again
some_word|

;; press M-b once
some_|word

;; word in camelCase (assuming subword-mode is not enabled)
|someWord

;; press M-f once
someWord|

;; word in camelCase (assuming subword-mode is enabled)
|someWord

;; press M-f once
some|Word
```

Personally I find the default behavior combined with `subword-mode`
great. I do a lot of Ruby and Lisp programming and it also makes a lot
of sense to me to be able to navigate the portions of a complex word,
but I guess not everyone feels this way. Enter `superword-mode` - when
it's enabled all "complex/compound" words are treated as a single word:

```
;; word with dash
|some-word

;; press M-f once
some-word|

;; word with underscore
|some_word

;; press M-f once
some_word|

;; word in camelCase
|someWord

;; press M-f once
someWord|
```

Note that you cannot have `subword-mode` and `superword-mode` enabled
at the same time. Turning one of them on will disable the other.

Personally, I don't see much value in `superword-mode` as a mode
that's enabled all the time, but I can imagine some useful
scenarios in which I'd enable it briefly to do some focused editing.
