---
layout: post
title: "Auto-indent your code with aggressive-indent-mode"
date: 2016-02-07 11:28
comments: true
tags:
- editing
---

One of the things I hate the most while programming, is having to
manually adjust the indentation of some code, after I've moved or
renamed something in it. While it's pretty easy to do such re-indent
operations using commands like `crux-indent-defun` or
advices like `crux-with-region-or-buffer` (you remember,
[crux](https://github.com/bbatsov/crux), right?), there's an even more
efficient way to tackle the issue at hand. Enter
[aggressive-indent-mode](https://github.com/Malabarba/aggressive-indent-mode).

`aggressive-indent-mode`'s name is a bit of a misnomer - it should
probably have been named `auto-indent-mode`, as this is what it
does. When you edit your code it will adjust the indentation
automatically. It's easier to show this than to explain it.

Here's one example showing `aggressive-indent-mode` enabled in `emacs-lisp-mode`:

![lisp example](https://raw.githubusercontent.com/Malabarba/aggressive-indent-mode/master/lisp-example.gif)

And another example using `cc-mode`:

![c example](https://raw.githubusercontent.com/Malabarba/aggressive-indent-mode/master/c-example.gif)

Provided you've installed the mode, enabling it for particular major modes is a piece of cake:

``` elisp
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'ruby-mode-hook #'aggressive-indent-mode)
```

If you want to enable it in all major modes you can do this as well:

``` elisp
(global-aggressive-indent-mode 1)
```

Note that this is not going to work well with modes like `python-mode`
and `haml-mode` where the proper indentation can't be reliably
determined.  When `global-aggressive-indent-mode` is enabled it will
not affect major modes listed in `aggressive-indent-excluded-modes`.

For more info - head over to the
[project's readme](https://github.com/Malabarba/aggressive-indent-mode/blob/master/README.md).
