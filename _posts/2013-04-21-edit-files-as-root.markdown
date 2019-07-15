---
layout: post
title: "Edit files as root"
date: 2013-04-21 10:03
comments: true
tags:
- Utilities
- crux
---

One area where Emacs traditionally falls short by default is editing
files that require `root` permissions. Since most Emacs users just use
a single Emacs frame they never leave, they have the problem of having
started it with their current user's privileges (and it's unlikely this
user is `root`). In this post I'll describe two ways to alleviate that
particular problem (note that `emacsclient` users have other options
to pick from as well - one is mentioned at the very end of the post).

<!--more-->

## Option A

Here's **Option A** - a simple command that allows you to reopen the
currently visited file with root permissions (obtained via `tramp` and
`sudo`) and prompts you for a file name if you're not currently
visiting a file or supply a prefix argument:

``` elisp
(defun er-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
```

I came across a similar command a few years back and it's been pretty
useful to me ever since. If you like it, I'd suggest binding it to `C-x
C-r` (kind of like `find-file`'s binding `C-x C-f`).

``` elisp
(global-set-key (kbd "C-x C-r") #'er-sudo-edit)
```

## Option B

Lately I've decided that such a command is a bit of an overhead, since
we can check the file permissions automatically anyways. While I'm not
quite fond of advising commands (debugging advised commands is no
fun) this was an excellent opportunity to exploit them (for great
good):

``` elisp
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
```

This advises `ido-find-file` (you might want to advise `find-file`
instead if you're not using `ido`) to reopen the selected file as
root (you'll be prompted for your `sudo` password) if you don't have
write permissions for it. Extremely cool!

The first command has been part of
[Prelude](https://github.com/bbatsov/prelude) since forever. As of late,
the `ido-find-file` advice is also present there and it
generally voids the need for `er-sudo-edit`.

## Option C

`emacsclient` users have it easy. They just need the following shell
alias (I generally alias `e` to `emacsclient -t`):

``` bash
alias E="SUDO_EDITOR=\"emacsclient -t -a emacs\" sudoedit"
```

This should (will) finally save you from reaching for `vim` in the terminal!

This command is available in [crux](https://github.com/bbatsov/crux) as
`crux-sudo-edit`. This command is also available in
[prelude](https://github.com/bbatsov/prelude) via the crux package.
