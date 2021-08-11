---
layout: post
title: Submitting a Package to NonGNU ELPA
date: 2021-08-11 10:25 +0300
tags:
- Packages
- NonGNU ELPA
---

Recently I wrote an [article about NonGNU ELPA]({% post_url 2021-08-02-nongnu-elpa-package-repository %}) and I promised to check for
myself how easy it is to submit a new package there. I made good on my promise and in this article I'll briefly describe the process.

I chose to use [clojure-mode](https://github.com/clojure-emacs/clojure-mode) for my small experiment, mostly because it doesn't have any dependencies, which simplifies the submission.[^1] Here's the submission process step by step:

* Clone NonGNU ELPA's git repository:

``` shellsession
$ git clone https://git.savannah.gnu.org/cgit/emacs/nongnu.git
```

* Create a new branch for your changes there:

``` shellsession
$ git checkout -B add-new-package
```

* Edit the file `elpa-packages` in the root of the repository. You basically need to add one section there for each package. Here's what I added for `clojure-mode`:

``` emacs-lisp
("clojure-mode"
 :url "https://github.com/clojure-emacs/clojure-mode"
 :ignored-files ("clojure-mode-extra-font-locking.el" "doc" "test" "test.clj")
 :readme "README.md"
 :news "CHANGELOG.md")
```

I believe the format is pretty self-explanatory, but you can read more about it [here](https://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/README?h=elpa-admin).
In particular there are some nuances about the default assumptions that you have to be aware of (e.g. for `:readme` this is README, README.rst, README.org or the commentary section of the package).

Note that it's important to tag your releases in git, so the build process will be able to identify them automatically. Otherwise you'll have to manually map git commit
to releases, as in this example:

``` emacs-lisp
("haskell-mode"	:url "https://github.com/haskell/haskell-mode"
 :doc "doc/haskell-mode.texi"
 :ignored-files ("images" "test" "logo.svg")
 ;; See https://github.com/haskell/haskell-mode/issues/1755
 ;;
 ;; Until a version tag is added, the commit of the latest tag is
 ;; used to determine the last stable release:
 ;; https://github.com/haskell/haskell-mode/releases/tag/17.2
 :version-map ((nil "17.2" "e72677668f5fc7cc148008e885a0f256e245dd43")))
```

* (Optional) Try to build your package locally. The process is [well-documented](https://git.savannah.gnu.org/cgit/emacs/nongnu.git/tree/README.org), so I won't
duplicate the instructions here. As most packages are trivial to build (e.g. they are a single Emacs Lisp file), the majority of the time you don't really have to verify this.

* Create a patch. This step might be a bit foreign to people who have only used git pull requests, but it's pretty simple:

``` shellsession
$ git format-patch
```

This will create a file named something like `0001-elpa-packages-clojure-mode-Add-package.patch`.

* Send the patch to the [emacs-devel](https://lists.gnu.org/mailman/listinfo/emacs-devel) mailing list. There your patch will be reviewed
and eventually applied by some of Emacs's maintainers.

That's it! I have to admit that the process is much simpler than I expected it
would be! Now all you have to do is wait for your package to be eventually
built and published. Here's the [end
result for clojure-mode](https://elpa.nongnu.org/nongnu/clojure-mode.html).  At this point you
can install `clojure-mode` from NonGNU ELPA!

That's all I have for you today. I hope this article will inspire some of you to submit their favorite packages to NonGNU ELPA! I've already submitted
patches for a few more packages that I maintain, and I hope to eventually have my entire Emacs "portfolio" available there. Keep hacking!

[^1]: Packages on NonGNU ELPA can only have dependencies available in GNU ELPA and NonGNU ELPA.
