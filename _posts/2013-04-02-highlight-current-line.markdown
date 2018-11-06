---
layout: post
title: "Highlight current line"
date: 2013-04-02 11:50
comments: true
tags:
- Editing
---

Just about every modern editor and IDE these days highlights the line
you're currently on, which I find extremely helpful. Emacs doesn't do
this out-of-the-box, but provides the means to do so - the built-in
global minor mode `global-hl-line-mode`. To take it out for a spin
just add the following line to your Emacs config:

``` elisp
(global-hl-line-mode +1)
```

In case you don't like the highlighting color you can change it by
adjusting the value of the `hl-line-face` face. Most decent color themes
pick a pretty good value for it, so normally you should not bother
changing it manually.

`global-hl-line-mode` is enabled out-of-the-box in
[Prelude](https://github.com/bbatsov/prelude).
