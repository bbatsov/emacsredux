---
layout: post
title: "which-function-mode"
date: 2014-04-05 15:33
comments: true
tags:
- Packages
---

Every developer is sometimes in the following situation - you're
inside the definition of something like a class or method, but the
name of the class or method is not currently into view, but you'd very
much like to know/see it. You can obviously scroll up and see what you
need to, but there's a simpler way to see the name you're looking
for - the built-in mode `which-function-mode`.

When `which-function-mode` is active you'll see in the center of your
modeline the name of the definition your cursor is currently in.
Here's the mode in action:

![which-func modeline](/assets/images/which-func-modeline.png)

And here's how to enable it:

``` elisp
(which-function-mode)
```

That way `which-function-mode` will be active in all major modes that
support it. If you want to enable it only in specific modes you can do
so like this:

``` elisp
(add-to-list 'which-func-modes 'ruby-mode)
(add-to-list 'which-func-modes 'emacs-lisp-mode)
```

By default `???` will be displayed when `which-function-mode` cannot
determine the name (perhaps because you're not actually in a
definition or due to implementation limitations). You can change this
to something else like this:

``` elisp
(setq which-func-unknown "n/a")
```

And what if you don't like to have the definition name displayed in
the mode-line? There's a solution for you as well (suggested by
[Sebastian Wiesner](http://www.lunaryorn.com/)). Use the following bit
of code:


``` elisp
;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
            ;; We remove Which Function Mode from the mode line, because it's mostly
            ;; invisible here anyway.
            (assq-delete-all 'which-func-mode mode-line-misc-info))
```

And you'll get the following result:

![which-func header](/assets/images/which-func-header.png)

That's all for today, folks! Hack Emacs Lisp & prosper!
