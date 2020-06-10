---
layout: post
title: Comment Commands Redux
author: Bozhidar Batsov
comments: true
tags:
- Editing
---

Comments are an important aspect of every programming language
and dealing with them effectively is an useful skill.

Emacs offers a bunch of comment-related commands and in this post
we're going to examine them.

<!--more-->

## comment-region

Let's start with `comment-region`. That's a very basic command that
will comment/uncomment the active region. Nothing fancy here.

The behaviour of this command can be tweaked by the variable `comment-style`.
All supported comment styles are defined in `comment-styles`:

``` emacs-lisp
((plain nil nil nil nil "Start in column 0 (do not indent), as in Emacs-20")
 (indent-or-triple nil nil nil multi-char "Start in column 0, but only for single-char starters")
 (indent nil nil nil t "Full comment per line, ends not aligned")
 (aligned nil t nil t "Full comment per line, ends aligned")
 (box nil t t t "Full comment per line, ends aligned, + top and bottom")
 (extra-line t nil t t "One comment for all lines, end on a line by itself")
 (multi-line t nil nil t "One comment for all lines, end on last commented line")
 (box-multi t t t t "One comment for all lines, + top and bottom"))
```

I'll admit I never felt the need to change the default value of `comment-style` (`indent`).

The `comment-region` command is not bound to any key by default. There's a good reason for this -
there are at least two more capable commands that render `comment-region` useless.

## uncomment-region

As the name implies it simply uncomments each line in the active region.

Like `comment-region` it's not bound to any key by default. In a similar vein - it's
a command you'd likely never use directly.

## comment-dwim

`comment-dwim`[^1] is the Swiss army knife of Emacs comment
commands. Depending on the context in which it's invoked it can
exhibit pretty different behaviours:

* If the region is active it calls `comment-region` (unless it only
consists of comments, in which case it calls `uncomment-region`).
* If the current line is empty, call
`comment-insert-comment-function` if it is defined, otherwise insert a
comment and indent it.
* If a prefix argument is specified (e.g. `C-u`), call
`comment-kill` (this command kills the first comment on the line, if any).
* Else, call `comment-indent`, which simply indents the comment.

This versatile command is bound to `M-;` by default.

## comment-line

`comment-line` is a newer command that was added in Emacs 25.1, as a simpler
alternative to `comment-dwim`. It will comment/uncomment the current line (or region) - nothing more, nothing less.

The command is bound to `C-x C-;`. Personally, I like it way more than
`comment-dwim`, as it's simpler and more consistent.

## comment-box

This command will put the selected region in a comment box. Here's an example:

``` emacs-lisp
(defgroup crux nil
  "crux configuration."
  :prefix "crux-"
  :group 'convenience)

;; becomes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defgroup crux nil      ;;
;;   "crux configuration." ;;
;;   :prefix "crux-"       ;;
;;   :group 'convenience)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```

This command is not bound to any key by default.

## Closing Thoughts

So many comment commands! On top of this there are packages like [comment-dwim-2](https://github.com/remyferre/comment-dwim-2) and some mode-specific
comment commands like the one provided by [clojure-comment-dwim](https://github.com/dotemacs/clojure-comment-dwim.el).

I've started writing this article about a year ago, I got interrupted by something, and I totally forgot about it. I'm pretty sure I meant to cover more
ground in it, but I now longer remember what I had in mind. :D I'm just happy I finally got to sharing it with you!

So, what's your approach for dealing with comments? Do you use some of the built-in commands or do you rely on custom commands or third-party extensions?

[^1]: `dwim` stands for "Do What I Mean".
