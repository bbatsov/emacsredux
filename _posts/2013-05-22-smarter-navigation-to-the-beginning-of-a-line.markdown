---
layout: post
title: "Smarter navigation to the beginning of a line"
date: 2013-05-22 17:48
comments: true
tags:
- Editing
- crux
---

In Emacs there are two essential commands when you have to go the
beginning of a line - `move-beginning-of-line`(bound to `C-a`) and
`back-to-indentation`(bound to `M-m`). The first takes you to the
first column of a line and the latter takes you the first non-whitespace
character on a line.

Generally, I find `back-to-indentation` more useful, but occasionally
it makes sense to go to the real beginning of a line as well. What
doesn't make sense is to have to think all the time what command is
the most appropriate in a particular situation. Wouldn't it be great
if `C-a` initially took you to the first non-whitespace char(as
`back-to-indentation` does) on a line, and if pressed again took
you to the actual beginning of the line? It would be! Let's get it
done:

``` elisp
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
```

The command will keep toggling between the first non-whitespace char
and the beginning of the line when invoked.

Here's a visual example(`|` is the cursor):

``` bash
This is a short example
    text|
    # pressing C-a once
   |text
    # pressing C-a again
|   text
    # pressing C-a again
   |text
```

This functionality could also be implemented with `defadvice`, but I tend to avoid their use.

This command is available in [crux](https://github.com/bbatsov/crux) as
`crux-move-beginning-of-line`. This command is also available in
[prelude](https://github.com/bbatsov/prelude) via the crux package.

**P.S.** The credit for this tip goes to [Sebastian Wiesner](https://github.com/lunaryorn).
