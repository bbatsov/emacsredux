---
layout: post
title: Hide Minor Modes in the Modeline in Emacs 31
date: 2025-12-24 10:14 +0200
tags:
- Emacs 31
---

Most Emacs users run a tone of minor modes and many of them contribute something
(usually useless) to the modeline. The problem is that the modeline is not
infinite and can quickly get quite cluttered. That's why for the longest time
I've been using the third-party `diminish` package and I have something like
this in my config:

```emacs-lisp
(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode))
```

`diminish` gets the job done, but it's a bit annoying that you need a
third-party package for something so basic. Fortunately that's about to
change...

I just learned that in Emacs 31 it's finally possible to hide minor modes in
the modeline using built-in functionality! Here's how you can do the
above:

```emacs-lisp
(setq mode-line-collapse-minor-modes '(abbrev-mode flyspell-mode flyspell-prog-mode eldoc-mode))
```

And here's how you can hide all minor modes (probably a bad idea, though, as
some add useful info to the modeline):

```emacs-lisp
(setq mode-line-collapse-minor-modes '(not))
```

For more info on what you can do with this new functionality see `C-h v mode-line-collapse-minor-modes`.
After all, they don't call Emacs the "self-documenting editor" for no reason.

From the docs you'll learn that hidden mode "lighters" (Emacs lingo for a mode's modeline indicator)
get compressed into one. It's `...` by default, but it can be customized via
the variable `mode-line-collapse-minor-modes-to`.

That's all I have for you today. Happy Christmas holidays! Keep hacking!
