---
layout: post
title: Comment Commands Redux
---

Comments are an important aspect of every programming language
and dealing with them effectively is an useful skill.

Emacs offers a bunch of comment-related commands and in this post
we're going to examine them.

<!--more-->

## comment-region

Let's start with `comment-region`. That's a very basic command that
will comment/uncomment the active region. Nothing fancy here.

The command is not bound to any key by default. There's a good reason for this -
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
`comment-kill`.
* Else, call `comment-indent`, which simply indents the comment.

This versatile command is bound to `M-;` by default.

## comment-line

`comment-line` is a newer command that was added in Emacs 25.1, as a simpler
alternative to `comment-dwim`. It will comment/uncomment the current line (or region) - nothing more, nothing less.

The command is bound to `C-x C-;`. Personally, I like it way more than
`comment-dwim`, as it's simpler and more consistent.

[^1]: `dwim` stands for "Do What I Mean".
