---
layout: post
title: Run Buffer-specific Commands with M-X
date: 2024-02-29 21:23 +0100
tags:
- Built-ins
- Emacs 29
---

No Emacs keybinding is more iconic than `M-x` (`execute-extented-command`) -
it allows you to run any Emacs command from the minibuffer and it sounds a lot
like Emacs![^1] It doesn't get better than this... or does it?

Emacs 29 introduced a new keybinding that's aiming to give `M-x` a run for its money - namely `M-X` (`execute-extended-command-for-buffer`). It's described like this:

> Query user for a command relevant for the current mode, and then execute it.
> This is like ‘execute-extended-command’, but it limits the
> completions to commands that are particularly relevant to the
> current buffer.  This includes commands that have been marked as
> being specially designed for the current major mode (and enabled
> minor modes), as well as commands bound in the active local key
> maps.

So, it basically narrows the list of commands that you can execute to those that
make the most sense in the context of the current buffer (mostly commands coming
from the current major mode and enabled minor modes). I can easily see it
becoming more useful than `M-x` when exploring some functionality that you're
not familiar with, as the results you'd get are narrower and you'd be more
likely to find what you're looking for.

That's all I have for you today. On February 29th we covered a small new feature in
Emacs 29. That makes me feel good! Keep hacking!

[^1]: Try pronouncing `M-x` out loud.
