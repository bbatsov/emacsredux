---
layout: post
title: GNU ELPA Devel Package Repository
date: 2021-07-27 10:05 +0300
tags:
- package.el
- ELPA
---

[GNU ELPA](https://elpa.gnu.org/) is one of the most well-known package repositories for Emacs (and the
only one enabled by default). Recently I learned there's also a GNU ELPA
repository for pre-release versions of the ELPA packages called
`elpa-devel`/`gnu-devel`. You can add it to your Emacs config like this if you
want to install the development version of some package (e.g. for test purposes
or to get a bug fix early):

``` emacs-lisp
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
```

You can think of this repository as something similar to the popular MELPA
repository, but limited only to the packages part of GNU ELPA.  It seems that
the repository is not mentioned anywhere in Emacs's documentation, that's why I
decided to write this short article about it. I doubt that most of you will ever need it,
but I guess it doesn't hurt to know about its existence.
