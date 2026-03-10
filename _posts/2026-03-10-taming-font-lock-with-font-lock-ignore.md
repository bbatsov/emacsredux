---
layout: post
title: Taming Font-Lock with font-lock-ignore
date: 2026-03-10 8:11 +0200
tags:
- Configuration
---

I recently wrote about
[customizing font-lock in the age of Tree-sitter]({% post_url 2026-03-08-customizing-font-lock-in-the-age-of-tree-sitter %}).
After publishing that article, a reader pointed out that I'd overlooked
`font-lock-ignore` -- a handy option for selectively disabling font-lock rules
that was introduced in Emacs 29. I'll admit I had no idea it existed, and I
figured if I missed it, I'm probably not the only one.[^1]

It's a bit amusing that something this useful only landed in Emacs 29 -- the very
release that kicked off the transition to Tree-sitter. Better late than never,
right?

## The Problem

Traditional font-lock gives you two ways to control highlighting: the coarse
`font-lock-maximum-decoration` (pick a level from 1 to 3) and the surgical
`font-lock-remove-keywords` (manually specify which keyword rules to drop). The
first is too blunt -- you can't say "I want level 3 but without operator
highlighting." The second is fragile -- you need to know the exact internal
structure of the mode's `font-lock-keywords` and call it from a mode hook.

What was missing was a declarative way to say "in this mode, don't highlight
these things" without getting your hands dirty with the internals. That's
exactly what `font-lock-ignore` provides.

## How It Works

`font-lock-ignore` is a single user option (a `defcustom`) whose value is an
alist. Each entry maps a mode symbol to a list of conditions that describe which
font-lock rules to suppress:

```emacs-lisp
(setq font-lock-ignore
      '((MODE CONDITION ...)
        (MODE CONDITION ...)
        ...))
```

**MODE** is a major or minor mode symbol. For major modes, `derived-mode-p` is
used, so a rule for `prog-mode` applies to all programming modes. For minor
modes, the rule applies when the mode is active.

**CONDITION** can be:

- **A face symbol** -- suppresses any font-lock rule that applies that face. Supports
  glob-style wildcards: `font-lock-*-face` matches all standard font-lock faces.
- **A string** -- suppresses any rule whose regexp would match that string. This
  lets you disable highlighting of a specific keyword like `"TODO"` or `"defun"`.
- **`(pred FUNCTION)`** -- suppresses rules for which `FUNCTION` returns non-nil.
- **`(not CONDITION)`**, **`(and CONDITION ...)`**, **`(or CONDITION ...)`** -- the
  usual logical combinators.
- **`(except CONDITION)`** -- carves out exceptions from broader rules.

**Note:** The Emacs manual covers `font-lock-ignore` in the
[Customizing Keywords](https://www.gnu.org/software/emacs/manual/html_node/elisp/Customizing-Keywords.html)
section of the Elisp reference.

## When to Use It

`font-lock-ignore` is most useful when you're generally happy with a mode's
highlighting but want to tone down specific aspects. Maybe you find type
annotations too noisy, or you don't want preprocessor directives highlighted, or
a minor mode is adding highlighting you don't care for.

For Tree-sitter modes, the feature/level system described in my
[previous article]({% post_url 2026-03-08-customizing-font-lock-in-the-age-of-tree-sitter %})
is the right tool for the job. But for traditional modes -- and there are still
plenty of those -- `font-lock-ignore` fills a gap that existed for decades.

## Discovering Which Faces to Suppress

To use `font-lock-ignore` effectively, you need to know which faces are being
applied to the text you want to change. A few built-in commands make this easy:

- **`C-u C-x =`** (`what-cursor-position` with a prefix argument) -- the quickest
  way. It shows the face at point along with other text properties right in the
  echo area.
- **`M-x describe-face`** -- prompts for a face name (defaulting to the face at
  point) and shows its full definition, inheritance chain, and current
  appearance.
- **`M-x list-faces-display`** -- opens a buffer listing all defined faces with
  visual samples. Handy for browsing the `font-lock-*-face` family and the newer
  Emacs 29 faces like `font-lock-bracket-face` and `font-lock-operator-face`.

Once you've identified the face, just drop it into `font-lock-ignore`.

## Practical Examples

Here's the example from the Emacs manual, which shows off the full range of
conditions:

```emacs-lisp
(setq font-lock-ignore
      '((prog-mode font-lock-*-face
                   (except help-echo))
        (emacs-lisp-mode (except ";;;###autoload"))
        (whitespace-mode whitespace-empty-at-bob-regexp)
        (makefile-mode (except *))))
```

Let's break it down:

1. In all `prog-mode` derivatives, suppress all standard `font-lock-*-face`
   highlighting (syntactic fontification for comments and strings is unaffected,
   since that uses the syntax table, not keyword rules).
2. But keep any rules that add a `help-echo` text property.
3. In `emacs-lisp-mode`, also keep the `;;;###autoload` cookie highlighting
   (which rule 1 would have suppressed).
4. When `whitespace-mode` is active, additionally suppress the
   `whitespace-empty-at-bob-regexp` highlight.
5. In `makefile-mode`, `(except *)` undoes all previous conditions, effectively
   exempting Makefiles from any filtering.

Here are some simpler, more focused examples:

```emacs-lisp
;; Disable type highlighting in all programming modes
(setq font-lock-ignore
      '((prog-mode font-lock-type-face)))

;; Disable bracket and operator faces specifically
(setq font-lock-ignore
      '((prog-mode font-lock-bracket-face
                   font-lock-operator-face)))

;; Disable keyword highlighting in python-mode only
(setq font-lock-ignore
      '((python-mode font-lock-keyword-face)))
```

Pretty sweet, right?

## Important Caveats

A few things to keep in mind:

- `font-lock-ignore` only affects **keyword fontification** (the regexp-based
  rules in `font-lock-keywords`). It does not touch **syntactic fontification** --
  comments and strings highlighted via the syntax table are not affected.
- It's a global option, not buffer-local. You scope rules to specific modes via
  the alist keys.
- Since it filters rules at compile time (during `font-lock-compile-keywords`),
  changes take effect the next time font-lock is initialized in a buffer. If
  you're experimenting, run `M-x font-lock-mode` twice (off then on) to see
  your changes.

## The End

I don't know about you, but I really wish that this got added to Emacs a long time
ago. Still, the transition to Tree-sitter modes is bound to take years, so many
of us will still get to leverage `font-lock-ignore` and benefit from it.

That's all I have for you today. Keep hacking!

[^1]: That's one of the reasons I love writing about Emacs features -- I often learn something new while doing the research for an article, and as bonus I get to learn from my readers as well.
