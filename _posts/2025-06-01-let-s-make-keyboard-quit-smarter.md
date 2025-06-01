---
layout: post
title: Let's make keyboard-quit smarter
date: 2025-06-01 23:39 +0300
tags:
- crux
- utils
---

I'll be pretty brief today. `keyboard-quit` (`C-g`) is one of the most
used commands, but unfortunately it's not very smart. Most annoyingly,
it doesn't work as expected when the minibuffer is active.

Fortunately, fixing such problems (and then some) is trivial in Emacs:

```emacs-lisp
(defun er-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))
```

As you've probably observed from the docstring, the new command addresses
3 shortcomings of the standard `keyboard-quit`.

I'd suggest to just remap `keyboard-quit` to our improved version:

```emacs-lisp
(global-set-key [remap keyboard-quit] #'er-keyboard-quit-dwim)
```

**Note:** The command shown in this article is bundled with
[crux](https://github.com/bbatsov/crux).

How would you improve `er-keyboard-quit-dwim` further?

That's all I have for you today! Keep hacking!
