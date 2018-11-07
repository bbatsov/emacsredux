---
layout: post
title: "Open File in External Program"
date: 2013-03-27 12:44
comments: true
tags:
- Utilities
- Crux
---

Sometimes it's useful to be able to open the file you're editing in
Emacs in an external program. For instance - you might be editing
some HTML file and you might want to see how is it looking in a
browser. I use the following handy command to do so:

``` elisp
(defun er-open-with (arg)
  "Open visited file in default external program.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))
```

On OS X it will use the built-in `open` program to decide which program
to open the file with. On Linux & *BSD it will use `xdg-open`. On all
other operating systems you'll be prompted to enter the name of the
program in the minibuffer (with autocompletion for the program name).
With a prefix argument you'll always be prompted for the name of the program to use.

I find it convenient to bind the command to `C-c o`:

``` elisp
(global-set-key (kbd "C-c o") #'er-open-with)
```

[crux](https://github.com/bbatsov/crux) features a variant of this
commands. (it is named `crux-open-with`).
