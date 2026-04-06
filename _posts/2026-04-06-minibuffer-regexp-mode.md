---
layout: post
title: "Live Regexp Feedback with minibuffer-regexp-mode"
date: 2026-04-06 12:00 +0300
tags:
- Emacs 30
- Regexp
---

This is the third article in a small series inspired by my recent cleanup of
[Prelude](https://github.com/bbatsov/prelude) and my [personal Emacs
configuration](https://github.com/bbatsov/emacs.d), following the ones on
[repeat-mode](/blog/2026/04/04/repeat-mode/) and
[read-extended-command-predicate](/blog/2026/04/04/read-extended-command-predicate/).
I've been going through the Emacs 28-30 changelogs for features I had ignored
so far, and this one from Emacs 30 immediately caught my eye.

Writing Emacs regexps has always been a bit of a dark art. Between the
double-escaped backslashes and the various group syntaxes (`\(...\)`,
`\(?:...\)`, `\(?N:...\)`), it's easy to lose track of what you're
actually matching. You type something into `query-replace-regexp`,
press RET, and hope for the best.

Emacs 30 added `minibuffer-regexp-mode`, a minor mode that gives you
live visual feedback as you compose a regexp in the minibuffer:

``` emacs-lisp
(minibuffer-regexp-mode 1)
```

<!--more-->

When active, the mode highlights the structure of your regexp *as you
type it* in the minibuffer. Capture groups, character classes, and
other constructs get color-coded so you can see at a glance whether
your grouping is right.

I find this particularly useful when building a regexp with
multiple groups for `query-replace-regexp`, where you need to get the
group numbering right for the replacement string (e.g., `\1`, `\2`).
The visual feedback makes it obvious which group is which.

## How Does This Compare to re-builder?

You might be wondering how this compares to `re-builder` (`M-x
re-builder`). They're complementary, really. `re-builder` shows
matches *in the buffer* as you type a regexp in a dedicated editing
window -- great for developing complex patterns against actual text.
`minibuffer-regexp-mode`, on the other hand, highlights the *regexp itself*
in the minibuffer. It kicks in automatically whenever you're prompted
for a regexp (e.g., `isearch-forward-regexp`, `query-replace-regexp`,
`keep-lines`, etc.).

One helps you see what your regexp matches; the other helps you see
what your regexp *says*. I'd suggest using both.

That's all I have for you today. Keep hacking!
