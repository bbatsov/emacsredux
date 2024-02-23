---
layout: post
title: Changing The Emacs Configuration Directory
date: 2024-02-23 15:36 +0100
tags:
- Emacs 29
---

I've noticed recently that I've missed one small, but very handy addition to Emacs 29 - the `--init-dir` command-line options. According the release notes:

> Emacs now supports setting 'user-emacs-directory' via '--init-directory'.  Use
> the '--init-directory' command-line option to set 'user-emacs-directory'.

Basically, this allows you to instruct Emacs where to read its configuration from. By default that's `.emacs.d`, but with this option you can easily override the default. That's extremely handy when you want to try out different Emacs setups. Here's some example:

``` shellsession
# Use Emacs Prelude
$ emacs --init-dir ~/emacs-distros/prelude

# Use Spacemacs
$ emacs --init-dir ~/emacs-distros/spacemacs
```

I'm guessing this command-line options will be enough for most people who casually switch between different configurations and will reduce the need for them to rely on more
sophisticated tools like [chemacs](https://github.com/plexus/chemacs2) or [with-emacs](https://github.com/alphapapa/with-emacs.sh).

Admittedly, I never used any such tools and I'd just symlink different dirs to `.emacs.d`, so `--init-dir` will definitely improve my workflow a bit.

That's all I have for you today! Keep hacking!
