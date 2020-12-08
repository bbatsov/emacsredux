---
layout: post
title: Favorite Emacs Packages
date: 2020-12-08 11:13 +0200
tags:
- Packages
---

People often ask me which are my favorite Emacs packages, so I've decided
to write a short article on the subject. I'll limit my myself to only 5 packages
and I'll exclude:

* built-in packages (e.g. `dired`, `erc`)
* color themes
* everything that's specific to a particular programming language (e.g. SLIME, CIDER)

In other words, I'll focus on (more or less) "universal" packages.

## Projectile

[Projectile](https://github.com/bbatsov/projectile) is a project interaction library, written by your truly.
I'm obviously biased here, but that's also the package I use the most in my day to day interactions with Emacs.

Projectile is a massive package with numerous commands, but I mostly limit myself to the ones outlined
[here](https://docs.projectile.mx/projectile/usage.html#basic-usage).

Funny enough, I think I never wrote any articles on Projectile here. I guess I should change this.

## Magit

[Magit](https://magit.vc/) is the best way to interact with Git from Emacs. Period.
It has probably converted more people to Emacs than any other Emacs package.

I'll have to admit that at this point I'm not sure whether I can still use Git from the command-line.

## Selectrum/Ivy

[Selectrum](https://github.com/raxod502/selectrum) and [Ivy](https://github.com/abo-abo/swiper#ivy) are minibuffer completion/filtering/sorting frameworks.
I'm cheating a bit here by listing both of them, but they are pretty similar and
equally awesome.

`Ivy` has more bells and whistles (more features and a fancier UI) and
`selectrum` has a simpler design. I used to use `ivy` for quite a while,
but recently I've switched to `selectrum`, as I realized I rarely used
`ivy`'s famous add-ons `swiper` and `counsel`.

Anyways, both packages are great and you can't go wrong choosing any them.

**Note:** Regardless of which one of them you're using, you definitely
want to combine them with the awesome
[prescient](https://github.com/raxod502/prescient.el) package that
supercharges the sorting and filtering algorithm.

## crux

[crux](https://github.com/bbatsov/crux) is a collection of ridiculously useful Emacs commands. Like the Matrix it cannot be explained, you have to experience it for yourselves.

**Note:** The package started life as an Emacs Prelude module, but was eventually extracted, so those commands could be used by anyone.
A lot of the articles I wrote early on at Emacs Redux were dedicated to commands that are part of `crux`.

## avy

[avy](https://github.com/abo-abo/avy) allows you to quickly (with only
a couple of keystrokes) jump to a specific place in your visible Emacs
windows.  I can't imagine going back to Emacs's standard commands for
those tasks, after spending so much time with `avy`.

## Honorary Mentions

I promised I'll limit myself to only 5 packages, but I cannot omit the following awesome packages as well:

* [company-mode](https://company-mode.github.io/) (a great completion framework)
* [diff-hl](https://github.com/dgutov/diff-hl) (shows you VC diffs in the gutter)
* [which-key](https://github.com/justbur/emacs-which-key) (helps you navigate the numerous Emacs keybindings with helpful hints in the minibuffer)
* [Smartparens](https://github.com/Fuco1/smartparens) (smart handling of paired delimiters; its `smartparens-strict-mode` is a decent paredit alternative)
* [Paredit](https://mumble.net/~campbell/emacs/paredit.html) (like smartparens, but geared towards Lisp programming)
* [undo-tree](http://www.dr-qubit.org/undo-tree.html) (my favourite way to deal with undo in Emacs; `undo-tree-visualize` is pure gold)
* [flycheck](https://www.flycheck.org) (lint tool integration)
* [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) (trying to give VS Code a run for its money)

I'll stop here, as I realized I use quite a few packages all the time.

## Closing Words

In my book, my favorite packages are ones that I'm using the
most. They keep changing with time, but I doubt I'll stop using any of
the packages that I mentioned in this article any time soon.

That being said, I've been using Emacs for over 15 years and looking
back at my list it seems to me that the only packages that I used back
then (circa 2005) where probably `paredit` and `undo-tree`. The Emacs
landscape has been very dynamic in recent years and everything has
changed (for the better). I can only imagine what amazing packages
will get created and become prominent in the next 15 years.

I didn't really cover any niche/obscure packages today, but I hope that some of you will learn about a new package
that they can experiment with. Playing with different packages and approaches to solving the same problem has always
been a defining trait of the Emacs experience for me.

That's all I have for you today. Down the road I plan to expand on some the packages I've mentioned in passing today.
So, what are your favorite Emacs packages?
