---
layout: post
title: "Customizing the Fringes"
date: 2015-01-18 09:41
comments: true
tags:
- appearance
---

On graphical displays, each Emacs window normally has narrow **fringes** (gutters/margins)
on the left and right edges. The fringes are used to display symbols
that provide information about the text in the window. You can type
`M-x fringe-mode` to disable the fringes, or modify their width. This
command affects fringes in all frames; to modify fringes on the
selected frame only, use `M-x set-fringe-style`. You can make your
changes to the fringes permanent by customizing the variable
`fringe-mode`.

Out-of-the-box the most common use of the fringes is to indicate a continuation
line. When one line of text is split into multiple screen lines, the
left fringe shows a curving arrow for each screen line except the
first, indicating that “this is not the real beginning”. The right
fringe shows a curving arrow for each screen line except the last,
indicating that “this is not the real end”. If the line's direction is
right-to-left, the meanings of the curving
arrows in the fringes are swapped.

Third-party modes like
[flycheck](https://github.com/flycheck/flycheck) and
[diff-hl](https://github.com/dgutov/diff-hl) also make use of the fringe to
display valuable information there (e.g. lint and VC information).

By default both fringes have width 8 pixels, but we can easily adjust this:

``` elisp
;; make both fringes 4 pixels wide
(fringe-mode 4)

;; make the left fringe 4 pixels wide and the right disappear
(fringe-mode '(4 . 0))

;; restore the default sizes
(fringe-mode nil)
```

As mentioned before, you can also invoke this command interactively and
determine the optimal fringe size for you, before making it permanent in
your config. The options presented by the `fring-mode` command are defined
in the `fringe-styles` list:

``` elisp
(defconst fringe-styles
  '(("default" . nil)
    ("no-fringes" . 0)
    ("right-only" . (0 . nil))
    ("left-only" . (nil . 0))
    ("half-width" . (4 . 4))
    ("minimal" . (1 . 1)))
  "Alist mapping fringe mode names to fringe widths.
Each list element has the form (NAME . WIDTH), where NAME is a
mnemonic fringe mode name and WIDTH is one of the following:
- nil, which means the default width (8 pixels).
- a cons cell (LEFT . RIGHT), where LEFT and RIGHT are
  respectively the left and right fringe widths in pixels, or
  nil (meaning the default width).
- a single integer, which specifies the pixel widths of both
fringes.")
```

Be careful when playing with the fringe size. Certain info doesn't
look very good when the fringe is too small (e.g. less than 4 pixels).
