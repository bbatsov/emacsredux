---
layout: post
title: "Use Tab to Indent or Complete"
date: 2016-01-31 09:02
comments: true
tags:
- Configuration
- Completion
---

By default in Emacs the `Tab` key does only indentation. If some major
mode provides completion of some form, you normally have to trigger it
with `M-Tab`. In most window managers, however, this keybinding is used to
switch between open windows, which makes it a bit hard to use out of the box.

There's a simple trick to augment the default `Tab` behavior. Just put
this in your Emacs config:

``` elisp
(setq tab-always-indent 'complete)
```

Now, when you press `Tab` one time it will indent and if you press it
again you'll get completion candidates. If the indentation at point is
already correct you'll get the completion candidates right away. As an
added bonus - you don't really need `M-Tab` anymore.

Simple and neat! One really has to wonder why this isn't the default
behavior.
