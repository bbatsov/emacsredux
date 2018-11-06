---
layout: post
title: "Continue a comment on the next line"
date: 2013-06-05 16:12
comments: true
tags:
- Editing
---

When writing code from time to time we have to write comments as
well. Most of the time programmers are using single-line comments,
denoted by some special starting character(s), like `#`, `//`,
etc. When you want to write a few lines of comments in Emacs you have two
options:

#### Option A

* Write the first line of the comment.
* Go to the next line.
* Insert the comment delimiter.
* Continue writing the comment.

#### Option B (editor's choice)

* White the first line of the comment.
* Press `M-j` (`indent-new-comment-line`).
* Continue writing the comment on the second line.

The command breaks the line at point and indents the following line,
continuing the comment if invoked within one. The indentation on the
second line matches the indentation on the first line.

``` ruby
# first line
#     indented line|(we press `M-j` here)
#     |(we get here after pressing `M-j`)
```

This command is intended for styles where you write a comment per
line, starting a new comment (and terminating it if necessary) on each
line.  If you want to continue one comment of the type `/* */` across
several lines, use `C-j` (`newline-and-indent`).

Outside of comments `M-j` behaves pretty much the same way as `C-j`.
