---
layout: post
title: "Kill whole line"
date: 2013-04-09 14:49
comments: true
tags:
- Editing
- crux
---

Let's continue our exploration of the fascinating topic of
[killing lines](/blog/2013/04/08/kill-line-backward/). Apart from
killing lines from the cursor to the end of the line(`kill-line`) and
killing lines from the cursor back to the beginning of the
line(`kill-line` with a `0` prefix argument) you can also kill entire
lines with a single command - `kill-whole-line`.

The command is bound to `C-S-Backspace` by default and kills the line
including its newline. With a prefix argument, it will kill that many
lines starting from the current line. If the argument is negative it
will kill that many lines backward plus the preceding newline. If the
argument is `0`, it will kill the current line but exclude the
trailing newline. We can improve the command a bit by making it
indentation aware(like we did for backward kill line in the previous post):

``` elisp
(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))
```

Now we can rebind `C-S-Backspace`.

``` elisp
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)
```

`remap` is a pretty neat trick in its own right; maybe I'll write a
bit more about it in the future.

This command is available in [crux](https://github.com/bbatsov/crux) as
`crux-kill-whole-line`. This command is also available in
[prelude](https://github.com/bbatsov/prelude) via the crux package.
