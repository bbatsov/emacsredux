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

In this list I have (great) fonts that are available by default on Windows,
macOS and Linux, so wherever I run my config it's certainly going to find a
decent font out-of-the-box. And because I'm not doing any checks like `(eq system-type
'gnu/linux)`, if I have installed my favorite font manually I'll end up using
it, regardless of the operating system in question.

We can make the font-selection code prettier, but that's not the point
today. All I want for you is to learn how useful the functions `find-fond` and
`font-spec` are when you're building a truly portable Emacs configuration. Keep hacking!
