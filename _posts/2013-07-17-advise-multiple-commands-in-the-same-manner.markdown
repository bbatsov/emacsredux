---
layout: post
title: "Advise multiple commands in the same manner"
date: 2013-07-17 16:15
comments: true
tags:
- Utilities
- Emacs Lisp
---

One of the well known features of
[Prelude](https://github.com/bbatsov/prelude) is that it saves buffers
with changes in them automatically when you jump between
windows. This is achieved with several simple `defadvice`s and without
going into many details the advice code for that feature might look like this:

``` elisp
;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before switch-to-buffer-auto-save activate)
  (prelude-auto-save))
(defadvice other-window (before other-window-auto-save activate)
  (prelude-auto-save))
(defadvice windmove-up (before other-window-auto-save activate)
  (prelude-auto-save))
(defadvice windmove-down (before other-window-auto-save activate)
  (prelude-auto-save))
(defadvice windmove-left (before other-window-auto-save activate)
  (prelude-auto-save))
(defadvice windmove-right (before other-window-auto-save activate)
  (prelude-auto-save))
```

Ouch - that a lot of redundant code! Luckily we can take care of the
redundancy by introducing a macro to generate multiple advices with
the same body:

``` elisp
(defmacro er-advise-commands (advice-name commands &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (before ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))
```

Looks a bit scary, doesn't it? But it allows us to reduce the original code down to:

``` elisp
;; advise all window switching functions
(er-advise-commands "auto-save"
                    (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
                    (prelude-auto-save))
```

`macroexpand` can show us how the macro gets expanded:

``` elisp
(macroexpand '(er-advise-commands "auto-save"
                 (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
                 (prelude-auto-save)))

(progn
  (defadvice switch-to-buffer
    (before switch-to-buffer-auto-save activate)
    (prelude-auto-save))
  (defadvice other-window
    (before other-window-auto-save activate)
    (prelude-auto-save))
  (defadvice windmove-up
    (before windmove-up-auto-save activate)
    (prelude-auto-save))
  (defadvice windmove-down
    (before windmove-down-auto-save activate)
    (prelude-auto-save))
  (defadvice windmove-left
    (before windmove-left-auto-save activate)
    (prelude-auto-save))
  (defadvice windmove-right
    (before windmove-right-auto-save activate)
    (prelude-auto-save)))
```

Obviously if we want the macro to be truly universal we should factor
out the hardcoded `before` and `activate` `defadvice` params, but
that's beside the point. The point is that when you need to generate
some code Emacs Lisp's macros have your back.
