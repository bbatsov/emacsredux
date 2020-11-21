---
layout: post
title: "Kill Other Buffers"
date: 2013-03-30 15:50
comments: true
tags:
- Utilities
- crux
---

Many text editors and IDEs offer the ability to close all open files
with the exception of the one you're currently in. Emacs does not.  At
least it doesn't until you add a snippet such as this one to your
Emacs setup:

``` elisp
(defun er-kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
```

Note that the command takes care not to kill *special* buffers (buffers
that do not correspond to files).

I'd suggest binding `kill-other-buffers` to `C-c k`.

``` elisp
(global-set-key (kbd "C-c k") #'er-kill-other-buffers)
```

The command can be implemented in a more elegant manner if one chooses to
leverage either the built-in `cl.el` library or the newer
[dash.el](https://github.com/magnars/dash.el). Here's the version of
the same command from [Prelude](https://github.com/bbatsov/prelude),
using `dash.el`:

``` elisp
(require 'dash)

(defun prelude-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (-each
   (->> (buffer-list)
     (-filter #'buffer-file-name)
     (--remove (eql (current-buffer) it)))
   #'kill-buffer))
```

According to your personal preference on functional programming the
second version might seem either much more elegant, ghastly or just the
same as the original.

This command is available in [crux](https://github.com/bbatsov/crux) as
`crux-kill-other-buffers`. This command is also available in
[Prelude](https://github.com/bbatsov/prelude) via the crux package.
