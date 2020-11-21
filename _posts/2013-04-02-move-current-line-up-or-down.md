---
layout: post
title: "Move current line up or down"
date: 2013-04-02 12:00
comments: true
tags:
- Editing
---

While programming(at least in some languages) I often find myself
wishing to drag the current line a couple lines up or down. Emacs does
have the means to transpose (switch) lines (courtesy of the
`transpose-lines` command bound to `C-x C-t`), but I find it
unwieldy. That's why I've built a couple of custom commands on top of
it:

``` elisp
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
```

Those are not the kind of commands you'll want to invoke with
`M-x`. Therefore I suggest binding them to something like `C-S-up` and
`C-S-down`(these bindings are often used for the same purpose in other
editors and IDEs):

``` elisp
(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)
```

Since this won't work on OSX(`Control+arrow` is used by the window
manager) I'd recommend the following alternative to OSX users:

``` elisp
(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)
```

Personally I'm not a fan of any keybindings that require me to move my
hands off the home keyboard row, but I wasn't able to come up with
anything better. Suggestions are welcome!

As usual both commands(and both set of keybindings) are available in
[Prelude](https://github.com/bbatsov/prelude)(but with `prelude-`
prefices).
