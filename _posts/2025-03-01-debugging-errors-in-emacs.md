---
layout: post
title: Debugging Errors in Emacs
date: 2025-03-01 18:36 +0200
tags:
- Debugging
---

I recently wrote an [article on debugging Emacs commands]({% post_url 2025-02-03-debugging-emacs-commands %}).
In it I mentioned `M-x toggle-debug-on-error` and `debug-on-error` briefly, but after posting the
article I realized that many people probably don't understand how
this works exactly.

The obvious thing that happens when `debug-on-error` is
enabled is that when an error happen you're seeing its backtrace (or stacktrace, depending on the terminology you prefer).
What's not so obvious (even, if it's in the name) is that this buffer is
actually a [debugger](https://www.gnu.org/software/emacs/manual/html_node/elisp/Debugger.html) buffer and you can do a lot with it. Here are a few examples:

1. **Navigate the Stack Trace**: Move your cursor in the `*Backtrace*` buffer to different lines representing various stack frames.
2. **Examine Local Variables**: Press `v` (`debugger-toggle-locals`) while on a stack frame to display local variables for that frame.
3. **Evaluate Expressions**: Use `e` (`debugger-eval-expression`) to evaluate Lisp expressions in the context of the current frame.
4. **Step Through Code**: Use `d` to step into function calls and evaluate expressions one by one.
5. **Continue Execution**: Press `c` to continue normal execution. (note, that unless you change something, this will result in the error you're trying to debug)
6. **Quit Debugging**: Enter `q` to exit the debugger and abort the current command.
7. **View Help**: Type `?` to see a list of available debugger commands.
8. **Record Evaluations**: Use `E` to evaluate an expression and save the result in the *Debugger-record* buffer.

It's important to understand that debugger runs in the environment of the error, allowing you to examine variable values precisely as they were at the time of the error.
This makes it a powerful tool for understanding and fixing issues in your Emacs Lisp code.

That debugging experience is one of the most powerful features of Lisps in general, and it's one of the reasons
why developing and debugging Emacs packages is pretty pleasant and productive experience. Provided you're
familiar with how to use the debugger in such cases that it.

You can more help inside Emacs by pressing `C-h m` while in a debugger buffer.

So, to recap - in case you run into some errors you should run the command `M-x toggle-debug-on-error`, re-run
whatever action caused the error and then navigate the stacktrace in the debugger to figure out what went wrong
exactly.

To debug an error that happens during loading of the init file, use the option
`--debug-init`. This binds `debug-on-error` to `t` while loading the init file, and
bypasses the `condition-case` which normally catches errors in the init file.

That's all I have for you today. I hope you've learned something useful today and next time you run into some error you'll be able to
fix it in no time!
