---
layout: post
title: Instant Access to Your Shell Init Files
date: 2020-07-16 13:01 +0300
tags:
- Crux
---

A long time I presented a [simple hack]({% post_url
2013-09-27-instant-access-to-your-shell-init-file %}) that allowed you to
quickly navigate to your shell's user config file (e.g. `.bashrc` or
`.zshrc`). While the solution gets the job done it was pretty basic and
limited - most notably it'd ignore the fact that you typically have several
shell config files that are often built on top of each other - e.g.  `/etc/profile`,
`~/.bash_profile` and `~/.bashrc`. Fortunately the original
hack evolved rather nicely and today lives in the
[crux](https://github.com/bbatsov/crux) library under the name
`crux-find-shell-init-file`.[^1]

Provided you've installed `crux` all you need to do is run that command
(e.g. with `M-x crux-find-shell-init-file`) and you'll get something like this
as the result:

![instant_shell_config.png](/assets/images/instant_shell_config.png)

Pretty neat, right? Even in its updated state the command is not complex at all:

``` emacs-lisp
(defun crux-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/" t))))
         (shell-init-file (cond
                           ((string= "zsh" shell) crux-shell-zsh-init-files)
                           ((string= "bash" shell) crux-shell-bash-init-files)
                           ((string= "tcsh" shell) crux-shell-tcsh-init-files)
                           ((string= "fish" shell) crux-shell-fish-init-files)
                           ((string-prefix-p "ksh" shell) crux-shell-ksh-init-files)
                           (t (error "Unknown shell"))))
         (candidates (cl-remove-if-not 'file-exists-p (mapcar 'substitute-in-file-name shell-init-file))))
    (if (> (length candidates) 1)
        (find-file-other-window (completing-read "Choose shell init file: " candidates))
      (find-file-other-window (car candidates)))))
```

I guess one thing that we can improve down the road is adding an option to
display the shell config in the same window, but that's a small thing.  The variables
like `crux-shell-bash-init-files` are simply lists of all potential files that
we should look for, that's why I've opted to omit them from the code listing.

I stand by my original suggestion to bind this useful command to `C-c S`:

``` elisp
(global-set-key (kbd "C-c S") #'crux-find-shell-init-file)
```

That's all I have for you today! Meta-x forever!

[^1]: Did you notice the subtle difference in the names of the original and the updated article?
