---
layout: post
title: Automatic Deindentation with kill-ring-deindent-mode
date: 2026-02-28 09:40 +0200
tags:
- Emacs 30
---

Here's a small Emacs 30 feature that solves an annoying problem you've probably
encountered hundreds of times without thinking about it.

You're reading some deeply nested code and you want to copy a chunk of it
somewhere — maybe into a message, a document, or a different part of the
codebase. You kill the region, yank it at the destination, and... it's indented
way too far because it carried along all the leading whitespace from its original
context. Then you manually fix the indentation. Every single time.

Enter `kill-ring-deindent-mode`.

## What it does

When this global minor mode is enabled, any text saved to the kill ring is
automatically de-indented based on the column position where the kill starts.
In other words, the common leading whitespace gets stripped.

For example, if you kill this indented block:

```c
        long_function (argument_1 (),
                       argument_2 (),
                       argument_3 ());
```

Instead of preserving all 8 columns of leading whitespace, the kill ring
receives:

```c
long_function (argument_1 (),
               argument_2 (),
               argument_3 ());
```

The relative indentation within the killed text is preserved — only the common
leading indentation is removed.

## Enabling it

```emacs-lisp
(kill-ring-deindent-mode 1)
```

That's it. No configuration needed. Once enabled, it works transparently with
every kill command — `C-w`, `M-w`, `C-k`, and so on.

## When is this useful?

- Copying code snippets into chat messages or emails
- Moving code between different nesting levels
- Yanking code into documentation or comments
- Pretty much any time you copy indented code

It's one of those features that you never knew you needed until you have it, and
then you can't imagine going back.

That's all I have for you today. Keep hacking!
