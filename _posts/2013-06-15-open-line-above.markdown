---
layout: post
title: "Open line above"
date: 2013-06-15 09:04
comments: true
tags:
- Editing
- crux
---

This post continues a topic that was introduced in
[smarter open-line](/blog/2013/03/26/smarter-open-line/) few months
back.

Often when editing code one wishes to open a line just above the
current one, which is properly indented relative to the existing code,
and position the cursor at its beginning.  Such a feature is present
in most IDEs, such as IntelliJ IDEA, Eclipse and NetBeans. It’s
sometimes bound to `Control+Shift+Enter`. Last time I showed you how
to implement a similar function called `smart-open-line`, this time
we will implement `smart-open-line-above`. Just add this snippet to your
`.emacs` (or `.emacs.d/init.el` or whatever):

``` elisp
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift return)] 'smart-open-line-above)
```

Evaluate the code (or restart Emacs) and you'll be able to use
`M-x smart-open-line-above` or `Control+Shift+Enter` (aka `C-S-return`).

Admittedly this keybinding kind of sucks, so here's another option for
you - `M-o` (used by default as the prefix for some font setting
commands nobody ever uses) for `smart-open-line` and `M-O` for
`smart-open-line-above`.

``` elisp
(global-set-key (kbd "M-o") 'smart-open-line)
(global-set-key (kbd "M-O") 'smart-open-line-above)
```

Another good option would be to fold the two commands into one and use a
prefix argument to trigger the opening a new line above the current
one.

These commands are available in [crux](https://github.com/bbatsov/crux) as
`crux-smart-open-line-above` and `crux-smart-open-line`.
These commands are also available in
[prelude](https://github.com/bbatsov/prelude) via the crux package.
