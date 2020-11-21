---
layout: post
title: "Erase Buffer"
date: 2013-05-04 10:44
comments: true
tags:
- Utilities
---

Some Emacs users find themselves wondering what's the fastest way to
erase the contents of a buffer. As you know, I'm fond of
[delete-selection-mode]({% post_url 2013-04-12-delete-selection-on-insert %})
and I find the combination of `C-x h` (`mark-whole-buffer`) + inserting
anything afterwards pretty potent.

Another pretty quick way to erase a buffer would be the built-in
`erase-buffer` command. It's disabled by default since it's considered
that beginners find it confusing(although I'm not sure why), so you'll
be prompted to enable it the first time you run `M-x
erase-buffer`. Alternative you can simply put this snippet in your
config:

``` elisp
;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)
```

You should note that any narrowing restrictions would have no effect
over `erase-buffer` and the buffer would be truly empty after invoking
the command. Conversely - narrowing has an effect over
`mark-whole-buffer`.

If you like the command you might want to bind it to some keybinding like `C-c E`:

``` elisp
(global-set-key (kbd "C-c E") #'erase-buffer)
```

Personally, I rarely need it, so I don't bind it to anything.

The `erase-buffer` command is enabled by default in
[Prelude](https://github.com/bbatsov/prelude).
