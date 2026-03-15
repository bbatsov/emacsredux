---
layout: post
title: "use-short-answers: The Modern Way to Tame yes-or-no Prompts"
date: 2026-03-15 07:10 +0200
tags:
- Emacs 28
- Configuration
---

I recently started a long overdue update of [Emacs
Prelude](https://github.com/bbatsov/prelude), rebasing it on Emacs 29 as the minimum supported
version. This has been a great excuse to revisit a bunch of old configuration patterns and
replace them with their modern built-in equivalents. One of the first things I updated was the
classic `yes-or-no-p` hack.

## The Problem

By default, Emacs asks you to type out the full word `yes` or `no` for certain prompts --
things like killing a modified buffer or deleting a file. The idea is that this extra friction
prevents you from accidentally confirming something destructive, but in practice most people
find it annoying and want to just hit `y` or `n`.

## The Old Way

For decades, the standard solution was one of these:

``` emacs-lisp
(fset 'yes-or-no-p 'y-or-n-p)

;; or equivalently:
(defalias 'yes-or-no-p 'y-or-n-p)
```

This worked by literally replacing the `yes-or-no-p` function with `y-or-n-p` at runtime. Hacky,
but effective -- until native compilation came along in Emacs 28 and broke it. Native compilation
can hardcode calls to C primitives, which means `fset`/`defalias` sometimes has no effect on
`yes-or-no-p` calls that were already compiled. You'd set it up, and some prompts would still
ask for `yes` or `no`. Not fun.

## The New Way

Emacs 28 introduced the `use-short-answers` variable:

``` emacs-lisp
(setopt use-short-answers t)
```

That's it. Clean, discoverable, native-compilation-safe, and officially supported. It makes
`yes-or-no-p` delegate to `y-or-n-p` internally, so it works correctly regardless of compilation
strategy.

If you're maintaining a config that needs to support older Emacs versions as well, you can do:

``` emacs-lisp
(if (boundp 'use-short-answers)
    (setopt use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))
```

## A Word of Caution

The Emacs maintainers intentionally designed `yes-or-no-p` to slow you down for destructive
operations. Enabling `use-short-answers` removes that friction entirely. In practice, I've never
accidentally confirmed something I shouldn't have with a quick `y`, but it's worth knowing the
tradeoff you're making.

## A Few More Things

If you're using GUI Emacs, you might also want to disable dialog boxes for a consistent
experience:

``` emacs-lisp
(setopt use-dialog-box nil)
```

It's also worth knowing that the related variable `read-answer-short` controls the same behavior
for multi-choice prompts (the ones using `read-answer` internally). Setting `use-short-answers`
affects both `yes-or-no-p` and `read-answer`.

This is one of those small quality-of-life improvements that Emacs has been accumulating in
recent versions. Updating Prelude has been a nice reminder of how many rough edges have been
smoothed over. Keep hacking!
