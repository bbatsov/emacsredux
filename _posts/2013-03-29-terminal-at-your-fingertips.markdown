---
layout: post
title: "Terminal at your fingertips"
date: 2013-03-29 15:38
comments: true
tags:
- Editing
---

Most of you probably know that you can run a terminal emulator(or
simply a shell for that matter) inside Emacs(`M-x ansi-term`).  This
is pretty neat on its own, but what I think is even better is to have
a quick way to jump to a particular terminal buffer reserved for the
occasional command invocation or two. I achieve this goal with the
following simple command:

``` elisp
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))
```

The first time you run the command it will create a new terminal
buffer based on your `SHELL` environment variable and display it in a
window adjacent to the one you're working in(in other words - it won't
clobber the window you are currently in). On successive invocations
the command will simply take you to that existing buffer (it will
still be displayed in a separate window).

Personally, I find this command extremely useful and therefore I bind
it to `C-c t`.

``` elisp
(global-set-key (kbd "C-c t") 'visit-term-buffer)
```

`visit-term-buffer` is available in
[Prelude](https://github.com/bbatsov/prelude)(but with a `prelude-`
prefix).
