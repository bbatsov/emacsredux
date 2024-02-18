---
layout: post
title: Lookup the Documentation of Functions, Variables and Faces
date: 2024-02-18 12:35 +0200
tags:
- Utils
---

Looking up the documentation for some command/function or configuration
option/variable is something all Emacs users have to do quite often.  Two of the
Emacs help commands that I use most often are `describe-function` (`C-h f`) and
`describe-variable` (`C-h v`). Basically they display the documentation for some
function or variable. E.g. if you press `C-h f` and type afterwards
`describe-function` we'd get the following:

```
describe-function is an autoloaded interactive compiled Lisp function in
‘help-fns.el’.

It is bound to C-h f, <f1> f, <help> f, <menu-bar> <help-menu> <describe>
<describe-function>.

(describe-function FUNCTION)

Display the full documentation of FUNCTION (a symbol).
When called from Lisp, FUNCTION may also be a function object.

See the ‘help-enable-symbol-autoload’ variable for special
handling of autoloaded functions.

  Probably introduced at or before Emacs version 22.1.
```

`describe-variable` is also useful to check the current value of some variable. Here's an example:

```
clojure-indent-keyword-style is a variable defined in ‘clojure-mode.el’.

Its value is ‘always-align’
```

I'm guessing most Emacs users are quite familiar with both commands. What I
didn't know until recently, though, is that Emacs 25.1 introduced the related
command `describe-symbol` (`C-h o`), which works for both functions and
variables. I'm guessing most people would benefit from using it over the more
specific commands, as it reduces some mental overhead. (the fewer keybindings we have to remember, the better)

Bonus points - `describe-symbol` also works with faces, so it's a good replacement
for `describe-face` (which doesn't have a default keybinding). One command to rule them all!

That's all I have for you today. Keep hacking!
