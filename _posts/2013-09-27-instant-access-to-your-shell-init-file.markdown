---
layout: post
title: "Instant Access to Your Shell Init File"
date: 2013-09-27 11:48
comments: true
tags:
- Utilities
- Emacs Lisp
- crux
---

A while ago I showed you a way to [instantly edit your Emacs init
file]({% post_url 2013-05-18-instant-access-to-init-dot-el %}). Today
we'll adapt the original idea for shell init files like `.bashrc` and
`.zshrc`. The code required is fairly short and simple:

``` elisp
(defun er-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))
```

The shell init file is deduced from your `SHELL` env variable. While
there are different shell init files for most shell
(e.g. `.bash_profile`, `.zshenv`, `.zprofile`), here we're assuming
you're using the most commonly used files. `find-file-other-window`
will open the file in a window adjacent to the one you're currently
in.

``` elisp
(global-set-key (kbd "C-c S") #'er-find-shell-init-file)
```

`er-find-shell-init-file` is available in
[crux](https://github.com/bbatsov/crux) (but with a `crux-`
prefix).
