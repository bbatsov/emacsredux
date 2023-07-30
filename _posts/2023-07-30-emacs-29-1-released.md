---
layout: post
title: Emacs 29.1 Released
date: 2023-07-30 11:20 +0300
tags:
- Announcements
- Emacs 29
---

Today is a great day for Emacs - Emacs 29.1 has just been released!
Every Emacs release is special, but I haven't been so excited about a new version of Emacs
in ages. Why so?

**Reason #1** - [pure GTK
front-end](https://batsov.com/articles/2021/12/06/emacs-is-not-a-proper-gtk-application/)
(a.k.a. `pgtk`). This also means that now Emacs supports natively Wayland. Which in tern means that it's easier than ever to run [Emacs in Windows's WSL]({% post_url 2021-12-19-using-emacs-on-windows-11-with-wsl2 %}). This is huge!

**Reason #2** - built-in support for the massively popular [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) via [eglot](https://github.com/joaotavora/eglot). `eglot` has existed for a while, but it's nice
to see it bundled with Emacs going forward. This will certainly make Emacs better positioned to complete with "modern" editors like VS Code.

**Reason #3** - built-in support for
[TreeSitter](https://tree-sitter.github.io/tree-sitter/). This means that a few
years down the road we'll have Emacs major modes that are much faster, robust
and feature-rich. It's infinitely easier to built a major mode using a real
parser instead of using regular expressions.  Lots of built-in modes have
already been updated to have a version using `TreeSitter` internally. Frankly, I
can't think of a bigger improvement in Emacs in the almost 20 years I've been an
Emacs user. Exciting times ahead!

You can read all about the new release [here](https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.29). I'll likely write a few articles about some of the new features in the weeks and months to come. In Emacs We Trust! M-x Forever!

**P.S.** Feel free to share in the comments what are you most excited about.
