---
layout: post
title: "Indent defun"
date: 2013-03-28 12:23
comments: true
tags:
- Editing
---

As a continuation of our previous topic of [indenting a buffer or a
region]({% post_url 2013-03-27-indent-region-or-buffer %}), this time around
I'll show you how to indent a single **defun**[^1]. So here's the magic
bit of code:

``` elisp
(defun er-indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))
```

This function should work in all properly written major programming modes.

Why would you want to use `er-indent-defun` instead of
`er-indent-region-or-buffer`? Pretty simple - indenting huge buffers can
take quite a while and you might want to save a bit of time if you
know that a buffer's indentation is correct pretty much everywhere
else.

I'd suggest binding the command to `C-M-z` (since it kind of resembles
the keybinding for function evaluation in major modes for dynamic
programming languages `C-M-x`).

``` elisp
(global-set-key (kbd "C-M-z") #'er-indent-defun)
```

Special thanks to `Fuco` [^2] who
suggested that command in the previous post.

`er-indent-defun` is available in
[crux](https://github.com/bbatsov/crux) (but with a `crux-`
prefix).

[^1]: Emacs slang/terminology for a function/procedure/method definition, originating from the way functions are defined in Emacs Lisp and a few other Lisp dialects.
[^2]: Who I assume is Matus Goljer, of [smartparens](https://github.com/Fuco1/smartparens) fame.
