---
layout: post
title: Redo Complex Command with Consult
date: 2021-11-25 13:15 +0200
tags:
- Consult
---

This article is a quick follow-up on yesterday's post on [redoing complex
commands]({% post_url 2021-11-24-redo-complex-command %}). I've noticed today
that the popular Emacs package [consult](https://github.com/minad/consult)
offers a much more powerful version of the built-in `repeat-complex-command`
command. The `consult` version is named `consult-complex-command` and has  a
couple of nice advantages:

* candidate filtering
* TAB-completion

![consult-complex-command.png](/assets/images/consult-complex-command.png)

Basically, it's lot faster to find what you're looking for. If you're into `consult` I'd suggest just using its version in place of the built-in command:

``` emacs-lisp
(global-set-key [remap repeat-complex-command] #'consult-complex-command)
```

You can invoke `consult-complex-command` with `C-x M-:` or `C-x ESC ESC`.

That's all I have for you today. The `consult` package is full of powerful versions
of built-in Emacs commands, so you'll do well to spend some time exploring it. I know I will!
