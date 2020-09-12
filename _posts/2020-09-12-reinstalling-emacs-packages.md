---
layout: post
title: Reinstalling Emacs Packages
date: 2020-09-12 20:35 +0300
tags:
- Utilities
---

From time to time you might run into issues with packages that are not properly
byte-compiled when installed via `package.el` (and `use-package` by
association). This may manifest itself in many different ways - typically
something being `nil`/undefined when it shouldn't be. Here's a small utility
that might help you in such situations:

``` emacs-lisp
(defun er-reinstall-package (package)
  (unload-feature package)
  (package-reinstall package)
  (require package))
```

I hope you'll agree that the function's definition is pretty self-explanatory. Now you can do things like:

``` emacs-lisp
(er-reinstall-package 'crux)
(er-reinstall-package 'projectile)
```

I assume that's not a function you'd need often, but it might come in handy from time to time.
That's all I have for you today. Keep hacking!
