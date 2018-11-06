---
layout: post
title: "A peek at Emacs 24.4: Smarter show-paren-mode"
date: 2013-12-31 12:09
comments: true
tags:
- Emacs24.4
---

Some time ago I wrote about
[highlighting matching delimiters with show-smartparens-mode](http://emacsredux.com/blog/2013/11/01/highlight-matching-delimiters-with-smartparens/). In
Emacs 24.4, the built-in `show-paren-mode` is capable of highlighting
more complex paired delimiters as well (like `do/end` in Ruby for
instance). There is one limitation to `show-paren-mode` (compared to
`show-smartparens-mode`) - it requires that the major mode, that it's
used together with (e.g. `ruby-mode`), is implemented in terms of the
new(ish) SMIE (Simple Minded Indentation Engine). SMIE has been around
since 23.3, but hasn't seen much adoption until quite recently. Prior
to Emacs 24.4 very few built-in modes were using it (and just about no
third-party major modes). In Emacs 24.4, however, a lot of modes were
updated to use SMIE (like `ruby-mode`), so you'll be able to enjoy the
`show-paren-mode` improvement with them.

Here's a glimpse of the enhanced `show-paren-mode` in action:

![show-paren-mode](/assets/images/show-paren-mode.gif)

Long term, I still think that betting on `smartparens-mode` is a good idea, but if
you prefer to stick with built-in modes - `show-paren-mode` is now more capable than ever.
