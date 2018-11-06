---
layout: post
title: "Automatic(electric) indentation"
date: 2013-03-29 12:09
comments: true
tags:
- Editing
---

Many newcomers dislike Emacs's default behavior of not indenting
automatically new lines in programming major modes. Usually one has to
press `Return`, followed by `Tab` to open a new line and indent it
according to its context.

Of course Emacs has a command named `newline-and-indent`, that's bound
to `C-j` and many people are into the habit of using it instead of the
pressing `Return+Tab`. Some even rebind `Return` to
`newline-and-indent`.

``` elisp
(global-set-key (kbd "RET") 'newline-and-indent)
```

What's little known is that Emacs 24.1 introduced a new global minor
mode called `electric-indent-mode`.  When enabled, typing certain
characters(like newlines) triggers reindentation. So you can simply
add this line to your Emacs config to get to enjoy
`electric-ident-mode`:

``` elisp
(electric-indent-mode +1)
```
