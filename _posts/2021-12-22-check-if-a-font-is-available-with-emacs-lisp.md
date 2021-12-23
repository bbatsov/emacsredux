---
layout: post
title: Check if a Font is Available with Emacs Lisp
date: 2021-12-22 16:45 +0200
tags:
- Fonts
- Emacs Lisp
---

If you are using the same configuration on multiple systems there's a chance that your preferred font is not available on some of them. That's why it's a good idea to check whether some font is around prior to trying to use it. Fortunately, that's trivial with Emacs Lisp:

``` emacs-lisp
(cond
 ((find-font (font-spec :name "Cascadia Code"))
  (set-frame-font "Cascadia Code-12"))
 ((find-font (font-spec :name "Menlo"))
  (set-frame-font "Menlo-12"))
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-frame-font "DejaVu Sans Mono-12"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata-12")))
```

Basically `font-spec` creates a font spec from whatever parameters are passed to it
and `find-font` will look for a font matching the font spec in question. For our
purposes all we need to do is use the `:name` parameter for `font-spec`.

In this list I have (great) fonts that are available by default on Windows,
macOS and Linux, so wherever I run my config it's certainly going to find a
decent font out-of-the-box. And because I'm not doing any checks like `(eq system-type
'gnu/linux)`, if I have installed my favorite font manually I'll end up using
it, regardless of the operating system in question.

We can make the font-selection code prettier, but that's not the point
today. All I want for you is to learn how useful the functions `find-font` and
`font-spec` are when you're building a truly portable Emacs configuration.
Probably the most important simplification we can do at this point is extracting a `fond-available-p` helper function:

``` emacs-lisp
(defun font-available-p (font-name)
  (find-font (font-spec :name font-name)))

(cond
 ((font-available-p "Cascadia Code")
  (set-frame-font "Cascadia Code-12"))
 ((font-available-p "Menlo")
  (set-frame-font "Menlo-12"))
 ((font-available-p "DejaVu Sans Mono")
  (set-frame-font "DejaVu Sans Mono-12"))
 ((font-available-p "Inconsolata")
  (set-frame-font "Inconsolata-12")))
```

By the way, that's not the only way to check if a font is available in
Emacs. Alternatively we can use something like this:

``` emacs-lisp
(member "Cascadia Code" (font-family-list))

(defun font-available-p (font-name)
  (member font-name (font-family-list)))
```

`font-family-list` returns a list of all the font families that are available,
so it's easy to simply check if some font is in this list. I'll leave it to you to decide which approach is easier/better.

That's all I have for you today. Keep hacking!
