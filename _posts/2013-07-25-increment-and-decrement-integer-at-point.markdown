---
layout: post
title: "Increment and decrement integer at point"
date: 2013-07-25 18:49
comments: true
tags:
- Utilities
---

While editing you often have to increment or decrement some number
(usually an integer) by some step. Obviously this is trivial when the
number is something like `10`, but not pretty pleasant when the number
is `2343566` and you want to increment it by `943`. Most of the time,
however, you'll probably be incrementing or decrementing by 1.

A long time ago I found a bit of code by
[Ryan Thompson](https://github.com/DarwinAwardWinner) to help us deal
with such tasks. Here's a slightly modified version of the original code:

``` elisp
(require 'thingatpt)

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))
```

The code is based on the popular built-in library `thing-at-point` and
extends it to make it aware of integer numbers.  The commands
`increment-integer-at-point` and `decrement-integer-at-point` operate
with a step of 1 by default, but with a prefix argument you can select
any step you desire. Unlike other similar commands floating in the
Internet, these two handle correctly numbers like `-3434` and
`+343`.

I'd suggest binding these commands to `C-c +` and `C-c -`:

``` elisp
(global-set-key (kbd "C-c +") 'increment-integer-at-point)
(global-set-key (kbd "C-c -") 'decrement-integer-at-point)
```

Both commands are available in
[Prelude](https://github.com/bbatsov/prelude)(but with a `prelude-`
prefix).
