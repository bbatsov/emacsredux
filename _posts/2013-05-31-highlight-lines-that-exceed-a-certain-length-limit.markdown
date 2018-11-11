---
layout: post
title: "Highlight Lines that Exceed a Certain Length Limit"
date: 2013-05-31 15:18
comments: true
tags:
- Configuration
---

When you're writing code you usually have to take into account the
programming language's convention for maximum line length. Most
programming languages urge hackers to keep line length under 80
characters(although in recent years it has often been argued that such
rules should be relaxed to 100-120 characters, given the state of
current computer displays).

There are many ways to highlight lines that exceed a certain length in
Emacs, but I find one to be particularly elegant - the use of the
built-in `whitespace-mode`. Most people use `whitespace-mode` to
visualize spaces, tabs and trailing whitespace, but it can actually do
a bit more that that. Here's the magic config:

``` elisp
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
```

The above snippet will enable `whitespace-mode` only in major modes
for programming. If you want to enable `whitespace-mode` everywhere
you might want to do this instead:

``` elisp
(global-whitespace-mode +1)
```

`whitespace-line-count` determines that maximum line length; feel free
to set this to whatever value suits you. `whitespace-style` determines
what kind of stuff `whitespace-mode` is going to highlight. At this
example we want to highlight only the part of lines exceeding the line
length limit. Take a look at `whitespace-style`'s documentation for
more details(`C-h v RET whitespace-style`).

Here's the result:

![long lines](/assets/images/long-lines.png)

It will probably come as no surprise that this functionality is
enabled out-of-the-box in [Prelude](https://github.com/bbatsov/prelude).
