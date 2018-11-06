---
layout: post
title: "Kill line backward"
date: 2013-04-08 18:33
comments: true
tags:
- Editing
---

Emacs does not have a command `backward-kill-line`(which would kill
the text from the point to the beginning of the line), but it doesn't
really need one anyways. Why so? Simple enough - invoking `kill-line`
with a prefix argument `0` does exactly the same thing!

```
C-0 C-k
M-0 C-k
C-u 0 C-k
```

Take your pick! If you'd rather have a quicker way to do backward line
killing you might consider rebinding `C-Backspace` or
`M-Backspace`(both are bound to `backward-word-kill` by
default). Personally I always do word killing with `M-Backspace`, so I
favor rebinding `C-Backspace`.

``` elisp
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)))
```

This command can be further improved if killing backward factors the
current indentation level:

``` elisp
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))
```

Thanks to [Steve Purcell](https://twitter.com/sanityinc) for
suggesting a similar command in the comments.

The `C-Backspace` keybinding is available out-of-the-box in
[Prelude](https://github.com/bbatsov/prelude).
