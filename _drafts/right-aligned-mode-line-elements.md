---
layout: post
title: Right-aligned Mode Line Elements
date: 2026-02-28 10:20 +0200
tags:
- Emacs 30
---

Emacs users have long wanted the ability to push certain mode line elements to
the right edge of the window. Think of it like having a left-aligned and
right-aligned section in your mode line — buffer name on the left, maybe the time
or some status info on the right.

Before Emacs 30, achieving this required various hacks involving padding
calculations and propertized strings. It was doable, but fragile and ugly.

Emacs 30 makes it trivial with `mode-line-format-right-align`.

## How it works

Just insert the symbol `mode-line-format-right-align` into your
`mode-line-format`. Everything before it is left-aligned (as usual), and
everything after it gets pushed to the right edge.

```emacs-lisp
(setq-default mode-line-format
              '("%e" mode-line-front-space
                mode-line-mule-info
                mode-line-modified
                " "
                mode-line-buffer-identification
                mode-line-format-right-align
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))
```

In this example, the buffer identification stays on the left, while modes and
miscellaneous info are pushed to the right.

## Controlling the alignment edge

By default, right-aligned elements are pushed to the extreme right of the
window. You can control this with `mode-line-right-align-edge`:

```emacs-lisp
;; Align to the left edge of the right fringe (default is 'window)
(setq mode-line-right-align-edge 'right-fringe)
```

The available values are:

| Value | Alignment point |
|-------|----------------|
| `window` | Extreme right edge of the window (default) |
| `right-fringe` | Left edge of the right fringe |
| `right-margin` | Left edge of the right margin |

## A practical example

Here's a minimal setup that puts the buffer name and modification status on the
left, and the current line/column and major mode on the right:

```emacs-lisp
(setq-default mode-line-format
              '(" %+ "
                mode-line-buffer-identification
                mode-line-format-right-align
                mode-line-position
                " "
                mode-line-modes))
```

Clean, readable, and no hacks required.

That's all I have for you today. Keep hacking!
