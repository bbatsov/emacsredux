---
layout: post
title: Automatically Kill Running Processes on Exit
date: 2020-07-18 22:20 +0300
tags:
- Basic Configuration
- Emacs 26
---

One of the things I really hate in Emacs is that prompt you normally get when you
try to exit Emacs when there are some sub-processes running (e.g. some external shell you've started).

I'm a big believer in just doing all the necessary exit cleanup automatically when the user requests an exit.
Simple as that. On a practical note - that prompt tends to interrupt the rather inept shutdown/restart
sequence of macOS, which waits for all applications to be gracefully closed before proceeding. The time
I've spent on macOS was the reason that prompted me to look for some solution of this issue.

So, can we do something about this? Historically we couldn't, but Emacs 26 finally introduced a configuration
setting for this - `confirm-kill-processes`. By default it's set to `t`, meaning you'll get
the confirmation prompt on exit, but you can easily change this:

``` emacs-lisp
(setq confirm-kill-processes nil)
```

Problem solved. That's all I have for you today.
