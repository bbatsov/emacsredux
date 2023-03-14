---
layout: post
title: Avoid Accidentally Minimizing Emacs
date: 2023-03-14 10:05 +0200
tags:
- Keybindings
---

One of the most annoying problems that newcomers experience with Emacs is that
they often press `C-z` out of habit when they want to undo something.[^1] `C-z` (and `C-x C-z`), however,
is bound to the command `suspend-frame` that works a bit differently than undo:

> Do whatever is right to suspend the current frame.
> Calls `suspend-emacs` if invoked from the controlling tty device,
> `suspend-tty` from a secondary tty device, and
> `iconify-or-deiconify-frame` from a graphical frame.

In short - in a terminal this will suspend Emacs and in a GUI it will minimize Emacs.
Both are annoying outcomes, if you didn't aim for them. So, what can we do about it?
Given the command is already bound to the "safer" `C-x C-z` as well, I think it's perfectly fine to just unbind it from `C-z`:

``` emacs-lisp
(global-unset-key (kbd "C-z"))
```

Or you can just bind it to `undo` for good measure and make learning Emacs a bit easier for you:

``` emacs-lisp
(global-set-key (kbd "C-z") #'undo)
```

Given that I never use neither `C-z` or `C-x C-z` I prefer to rebind both of them to `undo`. So, how do you feel about those keybindings? Do you do something about them?

That's all I have for you today. Keep hacking!

[^1]: I've been using Emacs for almost 20 years and I still press `C-z` by mistake, although for a different reason. I often the use the keybinding `C-c C-z` and sometimes by mistake I press `C-z` first or `C-x C-z`.
