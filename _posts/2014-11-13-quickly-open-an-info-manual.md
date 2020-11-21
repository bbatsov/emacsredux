---
layout: post
title: "Quickly Open an Info Manual"
date: 2014-11-13 15:25
comments: true
tags:
- info
---

Every Emacs user knows that Emacs ships with plenty of built-in
documentation in the `GNU info` format (they don't call it a
self-documenting editor for no reason). Most Emacs users know how to
access that built-in documentation with `C-h i` (`M-x info`) and some
Emacs users even know that the Emacs manual can be opened directly
with `C-h r` (`M-x info-emacs-manual`).

If you know the name of the manual you're looking for, however,
there's a nice little-known alternative to using `C-h i` - the
`info-display-manual` command. When you run it you'll be prompted in
the minibuffer for the name of the manual you'd like to view (manual
name completion is available).

To give you a more concrete example of the command's advantage over
`info` let's try to open the Emacs Lisp manual with both
commands. With `info` you'll have to type the following:

```
M-x info RET m elisp RET
```

And the alternative would be:

```
M-x info-emacs-manual RET elisp RET
```

If you like the command I'd suggest binding it to some keybinding:

``` elisp
(define-key 'help-command (kbd "C-i") 'info-display-manual)
```
