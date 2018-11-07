---
layout: post
title: "Copy Filename to the Clipboard"
date: 2013-03-27 12:17
comments: true
tags:
- Editing
---

Sometimes I need to copy the name of the currently visited file to the
clipboard.  Emacs does not have a built-in command for that, but
cooking one is pretty straightforward:

``` elisp
(defun er-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
```

Evaluate the new bit of code in Emacs (maybe by using `C-M-x`
somewhere in the body of the function definition) and invoke the
command with `M-x er-copy-file-name-to-clipboard`.
