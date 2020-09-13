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
(defun er-reinstall-package (pkg)
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))
```

I hope you'll agree that the function's definition is pretty self-explanatory. Now you can do things like:

``` emacs-lisp
;; try running this code in ielm or a scratch buffer
(er-reinstall-package 'crux)
(er-reinstall-package 'projectile)
```

As one reader suggested we can make the function a bit more user friendly by making it an interactive command that you can
invoke with `M-x`:

``` emacs-lisp
(defun er-reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))
```

Now you'll also get prompted for the package to reinstall with some convenient completion.

I assume that's not a function you'd need often, but it might come in handy from time to time.
That's all I have for you today. Keep hacking!
