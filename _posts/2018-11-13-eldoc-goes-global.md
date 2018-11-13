---
layout: post
title: Eldoc Goes Global
tags:
- Packages
- Emacs 25.1
---

I recently noticed that Emacs 25.1 had added a global variant of the
popular `eldoc-mode`, called `global-eldoc-mode`. What's more -
unlike `eldoc-mode`, `global-eldoc-mode` is enabled by default!

This means that you can get rid of all the code in your Emacs config that was
wiring up `eldoc-mode` for major modes that support it:


``` emacs-lisp
;; That code is now redundant
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
```

There are [some
reports](https://emacs.stackexchange.com/questions/31414/how-to-globally-disable-eldoc)
that `global-eldoc-mode` is causing performance issues in modes that
don't support it. I've never experienced this myself, but if you want
to disable it you can simply do so like this:

``` emacs-lisp
(global-eldoc-mode -1)
```

Now it's time to clean up my config! Deleting code always feels so good!
