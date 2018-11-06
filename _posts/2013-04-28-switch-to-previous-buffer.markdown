---
layout: post
title: "Switch to previous buffer"
date: 2013-04-28 08:43
comments: true
tags:
- Utilities
---

Jumping between the current buffer and the one you were in before is a
pretty common task. It's also one you can solve in a multitude of
way - the most popular being simply the use of `switch-to-buffer` and
`ido-switch-to-buffer`. Both commands (which are generally bound to
`C-x b`) would suggest to select the previous buffer when you invoke
them. While this was good enough for me for many years, recently
[Luke Randall](https://github.com/lukerandall) suggested me a neat
alternative:

``` elisp
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
```

When you pair this command with a handy
[key chord](/blog/2013/04/28/execute-commands-ninja-style-with-key-chord-mode/)
it becomes a great performance booster:

``` elisp
(key-chord-define-global "JJ" 'switch-to-previous-buffer)
```

If you're not fond of key chords I'd suggest a good old keybinding instead:

``` elisp
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)
```

The command and its key chord are part of [Prelude](https://github.com/bbatsov/prelude).
