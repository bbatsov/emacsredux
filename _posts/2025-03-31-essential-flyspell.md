---
layout: post
title: Essential Flyspell
date: 2025-03-31 22:52 +0300
tags:
- Flyspell
---

I've been a long time user of `flyspell-mode` and `flyspell-prog-mode`, but admittedly I
keep forgetting some of it's keybindings. And there aren't that many of them to begin with!

This article is my n-th attempt to help me memorize anything besides `C-c $`. So, here we go:

- `M-t` (`flyspell-auto-correct-word`) - press this while in word with typo in it to trigger auto-correct.
You can press it repeatedly to cycle through the list of candidates.
- `C-,` (`flyspell-goto-next-error`) - go to the next typo in the current buffer
- `C-.` (`flyspell-auto-correct-word`) - same as `M-t`
- `C-;` (`flyspell-auto-correct-previous-word`) - automatically correct the last misspelled word. (you can cycle here as well)

There are more commands, but those are the ones you really need to know.[^1]

If you ever forget any of them, just do a quick `C-h m RET flyspell-mode`.

Do you have some tips to share about using `flyspell-mode` (and maybe `ispell` as well)?

That's all I have for you today! Keep fixing those typos!

**P.S.** If you're completely new to `flyspell-mode`, you may want to check
[this article]({% post_url 2019-05-24-spell-checking-comments %}) on the subject as well.

[^1]: Unless you'd like to trigger auto-correct with your mouse, that is.
