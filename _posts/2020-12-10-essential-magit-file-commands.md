---
layout: post
title: Essential Magit File Commands
date: 2020-12-10 10:07 +0200
tags:
- Magit
---

Everyone knows [Magit](https://magit.vc/) and everyone knows it's one of [my favorite
Emacs packages]({% post_url 2020-12-08-favorite-emacs-packages %}).

One thing that probably fewer people know is that once Magit is installed,
it enables a global minor mode (`global-magit-file-mode`) that allows you to
interact with Magit from any Emacs buffer in 3 different ways:

* `C-x g` (`magit-status`) - that's the way in which most people use Magit.[^1] You get a dedicated Magit buffer where you can invoke all sorts of commands for everything that you can imagine (e.g. pulling, pushing, staging, committing, branching). Generally this seems to be the best way to work with multiple files.
* `C-x M-g` (`magit-dispatch`) - that's pretty much the same as `magit-status`, but you get the opportunity to trigger a Magit command directly from the minibuffer. One can argue that's a (slightly) more effective way to work if you know it advance what you want to do (e.g. pressing `C-x M-g l l` will display the git log).
* `C-c M-g` (`magit-file-dispatch`) - that's the way to invoke Magit commands on the current file (e.g. `blame`) and that's the hero of today's article.

I've noticed that for some reason many people don't use `magit-file-dispatch` much, which seems like a wasted opportunity as it provides that
fastest way to do common things like:

* `magit-blame` (`C-c M-g b`)
* stage the current file (`C-c M-g s`)
* commit the current file (`C-c M-g c`)
* show the git log for the current file (`C-c M-g l`)
* show the diff for the current file (`C-c M-g d`)

I hope you'll agree those are pretty handy commands. `magit-file-dispatch` offers other commands as well, but let's stick to the
essential ones today.

Magit probably has the best documentation of any Emacs package, but the problem with having a great documentation and lots of features
is that someone has to do a lot of reading. Consider this article a cheatsheet of sorts.
That's all I have for you today. I hope you've learned something useful! Feel free to share in the comments how/when do you use Magit's
3 modes of operations.

[^1]: A lot of people bind manually `C-x g` to `magit-status` in their Emacs configuration, but that hasn't been necessary for a while now.