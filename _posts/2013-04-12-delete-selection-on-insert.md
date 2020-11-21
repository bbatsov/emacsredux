---
layout: post
title: "Delete Selection on Insert"
date: 2013-04-12 11:57
comments: true
tags:
- Editing
---

It most text editors if you select a piece of text and press any
key (that's not bound to some command) the selection would be replaced
by the character bound to the key (or deleted if you press
`Backspace`).  In Emacs things look differently - by default typed
text is just inserted at point, regardless of any selection.

Of course, there is a way to change this:

``` elisp
(delete-selection-mode +1)
```

Now when `transient-mark-mode` is also enabled (it is enabled by
default since Emacs 23) any typed text replaces the selection if the
selection is active.

Personally I find this behavior more useful than the default
one. That's why it should be no surprise that `delete-selection-mode`
is enabled by default in
[Prelude](https://github.com/bbatsov/prelude).
