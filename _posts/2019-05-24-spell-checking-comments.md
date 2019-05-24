---
layout: post
title: Spell Checking Comments
date: 2019-05-24 09:07 +0200
tags:
- Flyspell
- Programming
---

I'm notorious for all the typos I make.[^1] Thankfully Emacs features an awesome built-in mode named `flyspell` to help poor typists like me.
Flyspell highlights misspelled words as you type (a.k.a. on the fly) and has useful keybindings to quickly fix them.

Most people typically enable `flyspell` only for major modes derived from `text-mode` (e.g. `markdown-mode`, `adoc-mode`), but it can really help programmers as well by
pointing out typos they make in comments. All you need to do is enable `flyspell-prog-mode`. I typically enable it for all programming modes[^2] like this:

``` emacs-lisp
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
```

Now you'll get instant feedback when you make some typo in a
comment. To fix a word just press `C-c $` (`M-x
flyspell-correct-word-before-point`), while your cursor is behind it.[^3]

![flyspell_prog_mode.gif](/assets/images/flyspell_prog_mode.gif)

That's all I have for you today! Keep fixing those nasty typos!

[^1]: Especially in blog posts.
[^2]: At least the well-behaved ones that derive from `prog-mode` that is.
[^3]: There are many other ways to correct misspelled words with flyspell, but we'll ignore them for now for the sake of simplicity.
