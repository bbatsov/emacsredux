---
layout: post
title: "ace-jump-mode is Dead, Long Live Avy"
date: 2015-07-19 11:48
comments: true
tags:
- Navigation
---

People often ask how am I navigating efficiently Emacs windows and
buffers. I have the feeling they expect me to share with them some
secrets that would turbo-charge common commands like `C-s`, `M-f`,
`C-x o`, etc.  I don't, however, use those commands that much. Ever
since I saw that vim's
[EasyMotion](https://github.com/easymotion/vim-easymotion) has been
ported to Emacs, I've been using that port - namely
[ace-jump-mode](https://github.com/winterTTr/ace-jump-mode).

Basically, it allows you to navigate to every visible portion of your
Emacs (buffers & windows) with only a handful of keystrokes (usually
one two activate it and one or two to get where you want to go). You
can see it in action in this
[excellent video](http://emacsrocks.com/e10.html). `ace-jump` served
me well for years, but I've had a few gripes with it that were never
addressed (multi-char targets, CamelCase support, etc). I would have
implemented those myself, if the project was maintained at all, but
alas - that's not the case. Seems I wasn't the only one who was
frustrated with `ace-jump`, as the prolific
[Oleh Krehel](http://oremacs.com/) reimplemented it pretty much from
scratch for the purposes of his excellent
[ace-window](https://github.com/abo-abo/ace-window) library. Once I
got wind of this, I managed to persuade Oleh to start distributing his
rewrite as a standalone project, which he dubbed
[avy](https://github.com/abo-abo/avy).

`Avy` features everything `ace-jump` does and more. Apart from the
many extra features, its codebase is way cleaner and readable and Oleh
is a fantastic and very responsive maintainer. So, as far as I'm
concerned `ace-jump` is now a dead project and pretty much everyone
who's using it should try out `avy` instead. Their usage and interface
are pretty similar, so the learning curve is non-existing. By the way,
here's `avy` in action:

![avy-go-to-char](https://raw.githubusercontent.com/wiki/nloyola/avy/images/avy-goto-char.png)

And what about my usage of `avy`? Nothing fancy here - I just bind the
commands I consider most important to handy keystrokes.

``` elisp
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-w") 'ace-window)
```

`avy-goto-word-or-subword-1` is aware of CamelCase words and I do a
lot of programming in languages that use those extensively.

`avy` has one more thing going for it - it's part of the default Emacs
package repo [GNU ELPA](https://elpa.gnu.org/packages/), which means
that you can install it right away without having to setup any
third-party repositories (which you'll probably need sooner or later).

`avy` and `ace-window` are naturally part of
[Prelude](https://github.com/bbatsov/prelude).

P.S. Oleh, one of those days you should rename `ace-window` to `avy-window`. :smile:
