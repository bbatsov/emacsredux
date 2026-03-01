---
layout: post
title: Soft Wrapping Done Right with visual-wrap-prefix-mode
date: 2026-03-01 07:39 +0200
tags:
- Emacs 30
- Built-ins
---

Emacs has always offered two camps when it comes to long lines: hard wrapping
(inserting actual newlines at `fill-column` with `M-q` or `auto-fill-mode`) and
soft wrapping (displaying long lines across multiple visual lines with
`visual-line-mode`).[^1]

Hard wrapping modifies the buffer text, which isn't always desirable. Soft
wrapping preserves the text but has always had one glaring problem:
continuation lines start at column 0, completely ignoring the indentation
context. This makes wrapped code and structured text look terrible.

Emacs 30 finally solves this with `visual-wrap-prefix-mode`.

## What it does

When enabled alongside `visual-line-mode`, `visual-wrap-prefix-mode`
automatically computes a `wrap-prefix` for each line based on its surrounding
context. Continuation lines are displayed with proper indentation — as if the
text had been filled with `M-q` — but without modifying the buffer at all.

The effect is purely visual. Your file stays untouched.

## Basic setup

As usual, you can enable the mode for a single buffer:

```emacs-lisp
(visual-wrap-prefix-mode 1)
```

Or globally:

```emacs-lisp
(global-visual-wrap-prefix-mode 1)
```

You'll likely want to pair it with `visual-line-mode`:

```emacs-lisp
(global-visual-line-mode 1)
(global-visual-wrap-prefix-mode 1)
```

Note that with `visual-line-mode` soft wrapping happens at the window edge. If
you'd like to control the extra indentation applied to continuation lines, you
can tweak `visual-wrap-extra-indent` (default `0`):

```emacs-lisp
;; Add 2 extra spaces of indentation to wrapped lines
(setq visual-wrap-extra-indent 2)
```

## Before and after

**Without `visual-wrap-prefix-mode`** (standard `visual-line-mode`):

```
    Some deeply indented text that is quite long and
wraps to the next line without any indentation, which
looks terrible and breaks the visual structure.
```

**With `visual-wrap-prefix-mode`:**

```
    Some deeply indented text that is quite long and
    wraps to the next line with proper indentation,
    preserving the visual structure nicely.
```

## A bit of history

If this sounds familiar, that's because it's essentially the `adaptive-wrap`
package from ELPA — renamed and integrated into core Emacs. If you've been using
`adaptive-wrap-prefix-mode`, you can now switch to the built-in version and drop
the external dependency.

## Closing Thoughts

As mentioned earlier, I'm not into soft wrapping myself - I hate long
lines and I prefer code to look exactly the same in every editor.
Still, sometimes you'll be dealing with some code you can't change,
and I guess many people don't feel as strongly about cross-editor
consistency as me. In those cases `visual-wrap-prefix-mode` will
be quite handy!

I have to admit I had forgotten about `auto-fill-mode` before doing the
research for this article - now I'm wondering why I'm not using it,
as pressing `M-q` all the time can get annoying.

That's all I have for you today. Keep hacking!

[^1]: I've always been in the `M-q` camp.
