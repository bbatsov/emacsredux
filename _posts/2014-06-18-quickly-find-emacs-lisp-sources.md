---
layout: post
title: "Quickly find Emacs Lisp sources"
date: 2014-06-18 16:26
comments: true
tags:
- misc
---

One thing Emacs users deal all the time is looking for the source of
various libraries and function or variable definitions.  Typically this
involves the use of commands like `C-h f` (`describe-function`), `C-h v`
(`describe-variable`) and `C-h k` (`describe-key`) and jumping to the source from the help buffer those commands produce.
Let's see if we can improve upon this workflow.

### find-func

Enter the built-in Emacs library `find-func`. It provides a myriad of useful commands to help us quickly locate sources.
Let's take a brief look at some of them.

* You can find the source of a library with `M-x find-library`. If you want to find the source of `ido` you can do so like this:

```
M-x find-library RET ido RET
```

I'd suggest binding the command to `C-h C-l` for quick access:

``` elisp
(define-key 'help-command (kbd "C-l") 'find-library)
```

* You can find the source of a function definition with `M-x find-function`. Here's an example:

```
M-x find-function RET find-function RET
```

I'd suggest binding the command to `C-h C-f` for quick access:

``` elisp
(define-key 'help-command (kbd "C-f") 'find-function)
```

We can actually do one better - we can directly jump to a command
definition using a keybinding of the command with `M-x
find-function-on-key`. Here's how we can find the source the command
bound to `C-a` (`beginning-of-line`):

```
M-x find-function-on-key RET C-a
```

I'd suggest binding the command to `C-h C-k` for quick access:

``` elisp
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
```

* You can find the source of a variable definition with `M-x find-variable`. Here's an example:

```
M-x find-variable RET large-file-warning-threshold RET
```

I'd suggest binding the command to `C-h C-v` for quick access:

``` elisp
(define-key 'help-command (kbd "C-v") 'find-variable)
```

***

The library provides other useful commands as well - like
`find-function-at-point` and `find-variable-at-point`.

If you don't like the keybindings I suggested you can use
`find-function-setup-keys` instead.  This will give you keybindings
like `C-x F`, `C-x V`, `C-f K` (plus a few extra for commands like
`find-funtion-other-window`).

### elisp-slime-nav

Another really cool way to browse Elisp sources (and documentation) is the third-party package
[elisp-slime-nav](https://github.com/purcell/elisp-slime-nav). Assuming you've installed it already you can
enabled it like this:

``` elisp
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
(add-hook hook 'elisp-slime-nav-mode))
```

Once this is done you'll be able to jump to the source of the Emacs
Lisp object at point (function or variable) with `M-.` (as in SLIME
and CIDER for Common Lisp and Clojure respectively) and jump back with
`M-,`. You can also see the description of the object at point using
`C-c C-d` (or `C-c C-d d`).

That's all for today, folks!

**P.S.**

All the suggested keybindings are present out-of-the-box in
[Prelude](https://github.com/bbatsov/prelude). `elisp-slime-nav` is also
enabled out-of-the box in Prelude.
