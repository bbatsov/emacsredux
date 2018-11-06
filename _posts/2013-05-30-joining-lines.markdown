---
layout: post
title: "Joining lines"
date: 2013-05-30 16:27
comments: true
tags:
- Editing
---

Often, while editing, you'll want to compress a few lines into
one. Here's a simple example to illustrate the problem at hand. We
want to convert this bit of Ruby code:

``` ruby
some_method(arg1,
            arg2,
            arg3)
```

into:

``` ruby
some_method(arg1, arg2, arg3)
```

Doing so is extremely easy. Just go to the last line in the first code
block and press 3 times `M-^` (`delete-indentation`, aliased
also to `join-line`).

If you're like me and like to join lines from top to bottom - go to
the first line and press `C-u M-^` three times. This works, but it is
kind of hard to press `C-u M-^` repeatedly, compared to just
`M-^`. Let's simplify this a bit:

``` elisp
(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
```

Isn't this a top way to join lines or what?

Since `delete-indentation` is bound both to `M-^` it makes
sense to use something like `C-^` for the new command:

``` elisp
(global-set-key (kbd "C-^") 'top-join-line)
```

That's mostly a personal preference I guess - feel free to use any other keycombo.

This command is part of
[Prelude](https://github.com/bbatsov/prelude)(it's named
`prelude-top-join-line` there) and it's bound to `C-^` there.
