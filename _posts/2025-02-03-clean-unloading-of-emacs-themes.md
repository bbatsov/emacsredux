---
layout: post
title: Clean Unloading of Emacs Themes
date: 2025-02-03 18:27 +0200
tags:
- Themes
- Utils
---

If you're like me, you probably like playing with new Emacs color themes from
time to time.  Sure, I'm the person behind the Emacs ports of
[Zenburn](https://github.com/bbatsov/zenburn-emacs) and
[Solarized](https://github.com/bbatsov/solarized-emacs), but as much as I like
them, even I get bored with them occasionally.[^1]

What I often do is to install a bunch of themes (e.g. using `M-x
package-install`) and to quickly try them out by evaluating snippets like the
ones below in Emacs:

```emacs-lisp
(load-theme 'catppuccin t)

(load-theme 'dracula t)

(load-theme 'gruvbox t)
```

One small problem with this, though, is that Emacs themes were designed in such
a way that one theme can be applied on top of another one. (loading a new theme
doesn't unload the previusly loaded one) In practice this often means that if you load
a few themes one after another they'll start to mess each other up. There are
several ways to address this, the simplest being to call `disable-theme` every
time before loading a new theme:

```emacs-lisp
(load-theme 'catppuccin t)

(disable-theme 'catppuccin)
(load-theme 'dracula t)

(disable-theme 'dracula)
(load-theme 'gruvbox t)
```

Or you can get more adventurous and create a small command that unloads all loaded themes, effectively
resetting the theme to Emacs's default one:

```emacs-lisp
(defun er-disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))
```

Now, you can simply do `M-x disable-all-active-themes` when you see
fit. Finally, you can consider creating a function complementary to `load-theme`
that unloads all active themes before loading the new one:

```emacs-lisp
(defun er-load-theme (theme)
  (er-disable-all-active-themes)
  (load-theme theme t))

(er-load-theme 'catppuccin t)

(er-load-theme 'dracula t)

(er-load-theme 'gruvbox t)
```

Pretty neat!

That's all I have for you on this subject. Keep hacking!

[^1]: Zenburn is to this day my all time favorite theme, though. I rarely manage to spend more than a few days away from it.
