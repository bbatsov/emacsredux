---
layout: post
title: Replacing dash.el with Built-in Emacs APIs
date: 2024-02-19 08:59 +0200
tags:
- Flycheck
- Emacs Lisp
- seq.el
- dash.el
---

`dash.el` has long been a staple in the Emacs community and I give it a lot of
credit for driving some upstream API progress. By showing how many people wanted
to use `dash.el` for various reasons (e.g. macros like `-if-let` and `-when-let`
and numerous functions that transform sequences), a strong case could be made
with Emacs's maintainers that core Emacs Lisp APIs needed to be
extended. Eventually that lead to the creation of `subr-x.el`[^1] (bundled with
Emacs since 24.3) and `seq.el` (bundled with Emacs since 26.1). The two packages
mostly obsolete `dash.el` and I guess this contributed to it being used less and
less these days.

I recently took over the maintenance of the popular
[Flycheck](https://github.com/flycheck/flycheck) package and I've noticed that
it was still using `dash.el`, despite targeting Emacs 26.1. As I plan to submit
Flycheck to the official NonGNU ELPA package repository, it couldn't have
dependencies that are not present on GNU ELPA and NonGNU ELPA and I needed to
remove `dash.el`. Doing so turned out to be trivial, as there were obvious built-in
alternatives for everything Flycheck was using:

- I replaced `-if-let(*)` and `-when-let(*)` with `if-let(*)` and `when-let(*)` from `subr-x.el`.
- I replaced `-any?` and `-first` with `seq-find`, `-all?` with `seq-every-p` and `-take` with `seq-take`.

And that was it! The whole process literally took me 10 minutes, most of them spent checking whether `seq-find` is the right replacement for `-any?` and `-first`.

So, all it all it was a very quick and easy process and I can totally recommend
to other packages that are still relying only on basic `dash.el` functionality
to replace it as well.  Now Flycheck is totally dependency-free and it's ready for submission
to NonGNU ELPA!

[^1]: Originally authored by yours truly.
