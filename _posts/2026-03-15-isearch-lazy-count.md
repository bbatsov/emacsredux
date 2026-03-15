---
layout: post
title: "isearch-lazy-count: Built-in Search Match Counting"
date: 2026-03-15 07:40 +0200
tags:
- Emacs 27
- Search
---

Continuing my [Prelude](https://github.com/bbatsov/prelude) modernization effort (see the
[previous post]({% post_url 2026-03-15-use-short-answers %}) for context), another
long-standing third-party dependency I was happy to drop was
[anzu](https://github.com/emacsorphanage/anzu).

## The Problem

When you're searching with `C-s` in Emacs, you can see the current match highlighted, but you
have no idea how many total matches exist in the buffer or which one you're currently on. Are
there 3 matches or 300? You just don't know.

## The Old Way

For years, I used the `anzu` package (an Emacs port of anzu.vim) to display match counts in the
mode-line. It worked well, but it was yet another third-party dependency to maintain -- and one
that eventually ended up in the [Emacs
orphanage](https://github.com/emacsorphanage), which is never a great sign for long-term
maintenance.

## The New Way

Emacs 27 introduced `isearch-lazy-count`:

``` emacs-lisp
(setopt isearch-lazy-count t)
```

With this enabled, your search prompt shows something like `(3/47)` -- meaning you're on the 3rd
match out of 47 total. Simple, built-in, and requires no external packages.

Unlike `anzu`, which showed counts in the mode-line, `isearch-lazy-count` displays them right in
the minibuffer alongside the search string, which is arguably a better location since your eyes
are already there while searching.

## Customizing the Format

Two variables control how the count is displayed:

``` emacs-lisp
;; Prefix format (default: "%s/%s ")
;; Shows before the search string in the minibuffer
(setopt lazy-count-prefix-format "(%s/%s) ")

;; Suffix format (default: nil)
;; Shows after the search string
(setopt lazy-count-suffix-format nil)
```

If you prefer the count at the end of the prompt (closer to how `anzu` felt), you can swap them:

``` emacs-lisp
(setopt lazy-count-prefix-format nil)
(setopt lazy-count-suffix-format " [%s/%s]")
```

## Good to Know

The count works with all isearch variants -- regular search, regex search (`C-M-s`), and word
search. It also shows counts during `query-replace` (`M-%`) and `query-replace-regexp`
(`C-M-%`), which is very handy for knowing how many replacements you're about to make.

The counting is "lazy" in the sense that it piggybacks on the lazy highlighting mechanism
(`lazy-highlight-mode`), so it doesn't add significant overhead. In very large buffers, you
might notice a brief delay before the count appears, controlled by
`lazy-highlight-initial-delay`. One thing to keep in mind -- if you've disabled lazy highlighting
for performance reasons, you'll need to re-enable it, as the count depends on it.

If you haven't read it already, check out my earlier article [You Have No Idea How Powerful
Isearch Is]({% post_url 2025-03-18-you-have-no-idea-how-powerful-isearch-is %}) for a deep dive
into what isearch can do. `isearch-lazy-count` pairs nicely with all the features covered there.

Between `use-short-answers` and `isearch-lazy-count`, that's two third-party packages I was able
to drop from Prelude just by using built-in functionality. Keep hacking!
