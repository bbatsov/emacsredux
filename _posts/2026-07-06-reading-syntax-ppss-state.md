---
layout: post
title: Reading syntax-ppss State Without the Magic Numbers
date: 2026-07-06 10:00 +0300
tags:
- Emacs Lisp
---

If you've ever written some Emacs Lisp that pokes around in a buffer, chances
are you've needed to answer a deceptively simple question at some point - is
point inside a string or a comment right now? It comes up constantly: when
you're font-locking something, when you're computing completion candidates, when
you're navigating code and want to skip over the bits that aren't actually code.

The workhorse behind all of this is `syntax-ppss`. The name is a bit of a
mouthful of abbreviations - it's short for "syntax parse-partial-sexp", and that
`ppss` tail stands for "parse-partial-sexp state", which is the value you get
back from Emacs's low-level `parse-partial-sexp` (itself short for "parse partial
s-expression"). Say that three times fast. Anyway, you hand `syntax-ppss` a
position (or just call it at point), it parses the buffer up to there and returns
that parser state - how deeply nested in parens you are, whether you're in a
string, where the enclosing list started, and so on.[^1]

There's just one catch. That parser state is a plain list of eleven elements,
and for the longest time the only way to get anything out of it was to reach in
by position:

``` elisp
;; are we inside a string?
(nth 3 (syntax-ppss))

;; ... or a comment?
(nth 4 (syntax-ppss))

;; where did that string or comment start?
(nth 8 (syntax-ppss))
```

I don't know about you, but I can never remember what lives at which index.
`nth 3` tells you precisely nothing about what you're checking. Every time I ran
into code like this I'd have to pull up `C-h f parse-partial-sexp RET` and count
elements in the docstring. Magic numbers scattered all over the place, and not
the fun kind.

Turns out Emacs 26.1 fixed this years ago and I somehow missed the memo. It
ships a `ppss` struct (yes, that abbreviation again) with proper named accessors
for every field. And here's
the neat part - the struct is defined with `:type list`, which means it's
literally the same list `syntax-ppss` already returns. No wrapping, no
conversion. You just call the accessor on the value you already have:

``` elisp
(ppss-string-terminator (syntax-ppss))       ; inside a string?
(ppss-comment-depth (syntax-ppss))           ; inside a comment?
(ppss-comment-or-string-start (syntax-ppss)) ; where it started
```

Now the code says what it means. These are the accessors I reach for the most,
though there's one for every slot in the list:

- `ppss-string-terminator` is non-nil when point sits inside a string (`nth 3`).
- `ppss-comment-depth` is non-nil inside a comment (`nth 4`).
- `ppss-comment-or-string-start` gives you the position where the current string or comment began, or nil if you're in neither (`nth 8`).
- `ppss-innermost-start` is the start of the innermost list containing point (`nth 1`), which is handy when you want to jump out to the enclosing form.
- `ppss-depth` is how many levels of parens deep you are (`nth 0`).

If you're worried about the cost of a function call on a hot path like
font-locking, don't be. The accessors come with compiler macros, so
`(ppss-string-terminator state)` compiles down to the exact same
`(nth 3 state)` you'd have typed by hand. You get the readable name at read time
and pay nothing at run time. And since `parse-partial-sexp` returns the same kind
of list, all of these work on its result too.

There's another helper worth knowing about for the most common check of all -
am I in a string or a comment, never mind which one. That's `syntax-ppss-context`,
which returns the symbol `string`, the symbol `comment`, or nil:

``` elisp
(if (syntax-ppss-context (syntax-ppss))
    (message "not in code")
  (message "somewhere in the code"))
```

A single call in place of the `(or (nth 3 state) (nth 4 state))` dance you see
all over the place. There's also `syntax-ppss-toplevel-pos` for getting back to
the enclosing top-level form. And plenty of major modes still bundle their own
little `foo-in-string-p` predicates - now you know what they're doing under the
hood.

The usual caveat about built-in libraries applies: the `ppss` accessors arrived
in Emacs 26.1, so if for some reason you're still targeting something older
you'll have to live with the `nth` calls. In 2026 that's unlikely to trouble
anyone.

I went on a little cleanup spree in [CIDER](https://github.com/clojure-emacs/cider)'s
codebase recently and swapped every one of these positional lookups for the named
accessors. It's the kind of change
that touches a lot of lines but risks nothing, and the diff ends up reading like
documentation. If you've got a stray `nth 3 (syntax-ppss)` lurking in your own
config or packages, it's a five-minute upgrade well worth making.

That's all I have for you today. Keep hacking!

[^1]: `syntax-ppss` is really just `parse-partial-sexp` run from the start of the top-level form down to your position, with a cache in front of it so that repeated calls at nearby positions stay cheap. That caching is why you should prefer it over calling `parse-partial-sexp` yourself most of the time.
