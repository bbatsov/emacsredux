---
layout: post
title: Run Shell Commands from the Minibuffer
date: 2021-04-07 10:14 +0300
tags:
- Shell
- Minibuffer
---

Today we're going back to the basics.

I assume most of you know that
you can run a shell within Emacs using `shell-mode` or even a
terminal (e.g. `ansi-term` or `vterm`). While those are super useful
there's a simpler and faster way to run the occasional shell command
via the aptly named command `shell-command`.

You can invoke `shell-command` by pressing `M-!` and you'll get a minibuffer
prompt asking you for the command to run. Type something like `ls -l` and you'll
the `ls` command's output straight in your minibuffer. Simple and sweet!

If the command you type ends in `&`, it will be executed asynchronously and
the output will appear in a dedicated buffer (`*Async Shell Command*`). This
is useful for commands that are going to take a while to complete. There's
also the command `async-shell-command` (bound to `M-&`) that always runs shell
command asynchronously.

I have to admit that I use these commands quite rarely, but they are still useful from time to time when I want to check something really quick without switching buffers.

As a reminder - you can also [evaluate Emacs Lisp code in the minibuffer]({% post_url 2013-04-18-evaluate-emacs-lisp-in-the-minibuffer %}).

That's all I have for you today. Keep hacking!
