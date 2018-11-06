---
layout: post
title: "Highlight matching delimiters with smartparens"
date: 2013-11-01 16:32
comments: true
tags:
- Editing
---

Some time ago I wrote about
[highlighting matching parentheses](http://emacsredux.com/blog/2013/04/01/highlight-matching-parentheses/)
with `show-paren-mode`. This is definitely useful, but it's kind of
restrictive, since parentheses are just a specific kind of paired
delimiter. Single and double quotes are also paired delimiters (at
least in most programmming languages). In the Ruby programmming
language, for instance, `do` and `end` also constitute paired
delimiter.

[smartparens](https://github.com/Fuco1/smartparens) offers
extremely customizable handling of paired delimiters and comes with an
extra minor mode called `show-smartparens-mode` to highlight
them. This mode totally replaces `show-paren-mode` and language
specific modes like `hirb` (which highlights Ruby blocks).

Assuming you're already using `smartparens`, enabling `show-smartparens-mode` is trivial:

``` elisp
(show-smartparens-global-mode +1)
```

Here's a glimpse of it in action:

![show-smartparens-mode](/assets/images/show-smartparens-mode.gif)

In [Prelude](https://github.com/bbatsov/prelude) `show-paren-mode` was
replaced by `show-smartparens-mode` some time ago.
