---
layout: post
title: Customizing Color Themes
date: 2025-02-13 10:23 +0200
tags:
- Themes
---

Every now and then you'd be trying out a new color theme, that you really like overall, but you'd
like to tweak a bit here and there to make it perfect. After all, that's what Emacs
is all about - creating the perfect editor for yourself.

Sometimes you might be dealing with missing face definitions or configuration
options that you might want to submit upstream, but most of the time the changes
you'd like to see are probably quite subjective and belong in your personal
config. So, how do you make those changes?

There are 3 common ways to adjust font faces in Emacs and I'll briefly cover all
of them. Option number 1 is the tried and true classic `custom-set-faces`:


``` emacs-lisp
(custom-set-faces
 '(region ((t (:inherit nil :background "RoyalBlue4"))))
 '(highlight ((t (:inherit region :background "dark olive green"))))
 '(hl-line ((t (:inherit highlight)))))
```

That's what gets generate if you're adjusting faces with something like `M-x customize-face`.
The bad thing about this approach is that those customizations will active regardless of your
currently selected color theme and if you like to switch themes that's not cool. Fortunately, it's
easily to narrow customizations to a particular theme with `custom-theme-set-faces`:

``` emacs-lisp
(custom-theme-set-faces
 'zenburn
 '(region ((t (:inherit nil :background "RoyalBlue4"))))
 '(highlight ((t (:inherit region :background "dark olive green"))))
 '(hl-line ((t (:inherit highlight)))))
```

Looking good!

**Note:** `custom-set-faces` works by calling `custom-theme-set-faces` for the `user`
theme, a special theme referring to settings made via Customize.

Finally, you can just set a specific face using `set-face-attribute` like this:

``` emacs-lisp
(set-face-attribute 'font-lock-builtin-face nil :weight 'bold)
```

I'd suggest perusing the documentation of `set-face-attribute` (e.g. with `C-h f`)
as it explains in great detail all the possible attributes you can configure
for a font face. The number of properties you can set is truly epic, but most of
the time you'll need to tweak only a couple of them. (e.g. `:foreground`, `:background`, etc)

Technically speaking, you can go a step further than that and define your own theme
that extends the color theme you want to modify[^1], but that's an overkill unless you
plan to distribute this theme as a package.

All the examples above are kind of random, so I'll conclude here with some real modifications
I do in my config to the popular Catppuccin theme:

``` emacs-lisp
(use-package catppuccin-theme
  :config
  ;; or 'latte, 'macchiato, or 'mocha
  (setq catppuccin-flavor 'macchiato)
  (load-theme 'catppuccin t)
  (custom-theme-set-faces
   'catppuccin
   ;; by default the theme uses the same face as for comments, which is wrong IMO
   '(font-lock-doc-face ((t (:foreground (catppuccin-color 'green)))))
   ;; font-lock variable definitions like function definitions
   '(font-lock-variable-face ((t (:inherit font-lock-function-face))))))
```

The example above also shows how to access the colors from the palette of some color theme
outside of its definition. Usually themes provide some API like `theme-name-color` to
get able to get the color codes easily.

**Note:** To see the new font faces in action you'll either have to restart Emacs or
evaluate Elisp code that sets them. (e.g. with `C-x C-e`)

One final tip - if you're wondering what's the face used by some text, the best
way to figure it out is with the `M-x describe-char` command. It will give you a ton of
information, including something like this near the end:

```
There are text properties here:
  face                 (font-lock-keyword-face markdown-code-face)
  font-lock-fontified  t
  font-lock-multiline  t
  fontified            t
  markdown-gfm-code    (2617 3092)
```

I had placed my cursor over the word "use-package" in the code snippet above, while writing
this article in `markdown-mode`, therefore the faces `font-lock-keyword-face` (coming from `elisp-mode`)
and `markdown-code-face` (from `markdown-mode`).

Do you have any tips on customizing color themes that you'd like share?

That's all I have for you today. Keep hacking!

[^1]: Remember that Emacs allows you load multiple themes with them stacking one upon another.
