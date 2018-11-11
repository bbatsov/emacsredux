---
layout: post
title: "Rename File and Buffer"
date: 2013-05-04 10:30
comments: true
tags:
- Utilities
- Emacs Lisp
- crux
---

A few weeks ago I wrote an article called ["Delete File and
Buffer"]({% post_url 2013-04-03-delete-file-and-buffer %}). Today
we'll revisit the article in a way, by exploring a pretty similar
topic - renaming of a file and its associated buffer. I've taken the
liberty to use pretty much the same wording I used in the
aforementioned post to spare me the effort of thinking up something
original.

<!--more-->

---

From time to time(most often when I refactor code) I need to
quickly(this removes `dired` from the equation) rename a file and
the buffer associated with it. Since most of the files I work with
are under version control I can just use the tried and true `M-x
vc-rename-file`. Unfortunately the command does not act on the current
file and will prompt you for a file to rename. Looks like we need to
create a simple wrapper around it to get the job done:

``` elisp
(defun er-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))
```

The wrapper is extra smart and will work on files that are not under
version control as well! I'm pretty fond of commands that do what you
mean instead of throwing errors. Now that we have this neat little
command we should probably bind it to some each to press keys, like `C-c r`:

``` elisp
(global-set-key (kbd "C-c r")  #'er-rename-file-and-buffer)
```

As usual both the command and its keybinding are available in
[Prelude](https://github.com/bbatsov/prelude).[^1]

[^1]: Technically speaking the command is part of [crux](https://github.com/bbatsov/crux).
