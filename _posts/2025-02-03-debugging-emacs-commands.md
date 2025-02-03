---
layout: post
title: Debugging Emacs Commands
date: 2025-02-03 12:20 +0200
tags:
- Debugging
- Troubleshooting
---

If you're using Emacs long enough sooner or later you'll run into some
Emacs command that's misbehaving and you'll need to debug it somehow.[^1]

If the command is just blowing up, probably you'll be able to figure out
what's going on by looking at its backtrace. To get meaningful backtraces
you can either run `M-x toggle-debug-on-error` or add this to your Emacs config:

```elisp
(setq debug-on-error t)
```

Sometimes that will be enough, but other times you'll need to dig deeper...
Fortunately for us Emacs features a super powerful built-in
[Emacs Lisp debugger](http://www.gnu.org/software/emacs/manual/html_node/elisp/Edebug.html)
and using it is the best way to diagnose problems of any kind.[^2]

To debug some command you need to do the following:

* Figure out the name of the command you want to debug (e.g. by using `C-h k`
to see which command is associated with some keybinding)
* Find the source of the command (e.g. by using `M-x find-function RET function-name`)
* Press `C-u C-M-x` while in the body of the function
* Run the command again

At this point you'll be dropped in the debugger and you can step forward (by pressing `n`) until
you find the problem.

I use this approach all time and it's very efficient. It's also not specific to
commands and works great for all Emacs Lisp code. Every Emacs user will do well
to invest a bit of time into learning the basics of debugging Emacs Lisp code.

That's all I have for you today. Keep hacking!

[^1]: Emacs commands are simply Emacs Lisp functions that can be invoked interactively with `M-x`.
[^2]: Here's a [great crash course](https://www.youtube.com/watch?v=odkYXXYOxpo) on using the debugger.
