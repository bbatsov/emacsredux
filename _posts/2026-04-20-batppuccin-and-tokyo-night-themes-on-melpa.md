---
layout: post
title: "Batppuccin and Tokyo Night Themes Land on MELPA"
date: 2026-04-20 10:00 +0300
tags:
- Themes
- Packages
---

Quick heads-up: my two newest Emacs themes are now on MELPA, so
installing them is a plain old `package-install` away.

- [batppuccin](https://melpa.org/#/batppuccin) is my take on the popular
  [Catppuccin](https://github.com/catppuccin/catppuccin) palette. Four
  flavors (mocha, macchiato, frappe, latte) across the dark-to-light
  spectrum, each defined as a proper `deftheme` that plays nicely with
  `load-theme` and theme-switching packages.
- [tokyo-night](https://melpa.org/#/tokyo-night) is a faithful port of
  [folke's Tokyo Night](https://github.com/folke/tokyonight.nvim), with all
  four upstream variants included (night, storm, moon, day).

Both themes come with broad face coverage out of the box (e.g. magit, vertico,
corfu, marginalia, transient, flycheck, doom-modeline, and many, many more),
a shared palette file per package, and the usual `*-select`, `*-reload`,
and `*-list-colors` helpers.

Installation is now as simple as you'd expect:

``` emacs-lisp
(use-package batppuccin-theme
  :ensure t
  :config
  (load-theme 'batppuccin-mocha t))

(use-package tokyo-night-theme
  :ensure t
  :config
  (load-theme 'tokyo-night t))
```

If you're curious about the design decisions behind these themes, I've
covered the rationale in a couple of earlier posts. [Batppuccin: My Take
on Catppuccin for Emacs](https://batsov.com/articles/2026/03/29/batppuccin-my-take-on-catppuccin-for-emacs/)
explains why I bothered with another Catppuccin port when an official
one already exists. [Creating Emacs Color Themes, Revisited]({% post_url 2026-03-30-creating-emacs-color-themes %})
zooms out to the broader topic of building and maintaining Emacs themes
in 2026.

Give them a spin and let me know what you think. That's all I have for you
today. Keep hacking!
