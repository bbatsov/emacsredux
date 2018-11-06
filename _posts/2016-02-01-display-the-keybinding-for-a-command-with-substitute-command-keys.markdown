---
layout: post
title: "Display the Keybinding for a Command with substitute-command-keys"
date: 2016-02-01 08:23
comments: true
tags:
- keybindings
---

If you ever need to show the keybinding for a particular command to
the users of your package (e.g. you're adding some tips
functionality), you should avoid resisting the urge to write something like
this:

``` elisp
(message "Press <C-c p p> to switch between projects.")
```

Why is this a bad idea? Because you might change the keybinding of the
command in question (e.g. `projectile-switch-project`, but you might
forget to update messages like this. Is there a better way?
`substitute-command-keys` to the rescue:

``` elisp
(message (substitute-command-keys "Press <\\[projectile-switch-project]> to switch between projects"))
```

This will produce exactly the same message as before and you're
guaranteed the keybinding will always be in sync with the command.

Neat!

P.S. If you want to check interactively the keybinding of some command use `C-h f` (`describe-function`). Here's
an example - `C-h f RET projectile-switch-project RET` will produce this:

```
projectile-switch-project is an interactive compiled Lisp function.

It is bound to C-c p p, s-p p, <menu-bar> <tools> <Projectile> <Switch
to project>.

(projectile-switch-project &optional ARG1)

...
```

You can also check which command is bound to some key with `C-h k`.
