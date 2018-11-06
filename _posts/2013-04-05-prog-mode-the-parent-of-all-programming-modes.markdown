---
layout: post
title: "prog-mode: The parent of all programming modes"
date: 2013-04-05 12:04
comments: true
tags:
- Programming
---

You probably know that major modes in Emacs can derive(inherit)
functionality from a parent major mode.

Emacs 24.1 introduced `prog-mode` - a new major mode from which all
programming modes should be derived. All of the programming modes that
come bundled with Emacs already inherit it(as do most
actively maintained third-party modes). Having a common parent for all
the programming modes allows us to do some pretty neat stuff - like
setting common behavior for all of them. Here's how we can enable
spell-checking in comments:

``` elisp
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
```

That's all for now. We'll discuss `flyspell` in more detail down the road.
