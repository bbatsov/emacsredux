---
layout: post
title: "Delete file and buffer"
date: 2013-04-03 12:24
comments: true
tags:
- Utilities
---

From time to time(most often when I refactor code) I need to
quickly(this removes `dired` from the equation) delete a file and kill
the buffer associated with it. Since most of the files I work with
are under version control I can just use the tried and true `M-x
vc-delete-file`. Unfortunately the command does not act on the current
file and will prompt you for a file to delete. Looks like we need to
create a simple wrapper around it to get the job done:

``` elisp
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

```

The wrapper is extra smart and will work on files that are not under
version control as well! I'm pretty fond of commands that do what you
mean instead of throwing errors. Now that we have this neat little
command we should probably bind it to some each to press keys, like `C-c D`:

``` elisp
(global-set-key (kbd "C-c D")  'delete-file-and-buffer)
```

As usual both the command and its keybinding are available in
[Prelude](https://github.com/bbatsov/prelude).
