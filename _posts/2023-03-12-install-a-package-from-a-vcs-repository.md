---
layout: post
title: Install a Package from a VCS Repository
date: 2023-03-12 10:40 +0200
tags:
- Emacs
- Packages
---

Most of the time the packages we need to install are already available in some
of the popular package repositories (e.g. MELPA or GNU ELPA). Occasionally, however,
some Elisp code has not yet been packaged (or we want to use an unreleased version of a package) and this forces us to be a bit more creative with respect to installing it. There has always been several ways to approach this, but Emacs 29 adds one more option that's super easy to use - `package-vc-install`.

When used interactive the command will prompt you for the repository from which to install a package. Alternative you can use it programatically like this:

``` emacs-lisp
(package-vc-install "https://github.com/clojure-emacs/clojure-ts-mode")
```

The command will try to infer the name of the package from the URL of the
repository you've supplied.  Upon closer inspection you'll also notice that the
command takes a few more parameters that allow you to specify the name of the
package and a particular revision (commit) you want to use:

``` emacs-lisp
(package-vc-install PACKAGE &optional NAME REV BACKEND)
```

I've yet to play more with this command, but I guess it might serve as a simple alternative to tools like `quelpa` or `straight.el`, if you're not using much of their functionality. And this certainly beats copying manually Elisp files from VCS repositories.

From my perspective the main use-case for the new command will be pinning packages to some specific revision, as sadly it is still not possible to install older revisions of Emacs packages from package repositories (they carry only the latest version of a package). A feature that's occasionally useful when you run into regressions with the latest version.

That's all I have for you today. Keep hacking!
