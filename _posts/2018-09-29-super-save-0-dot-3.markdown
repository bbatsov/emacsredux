---
layout: post
title: "super-save 0.3"
date: 2018-09-29 20:22
comments: true
tags:
- Packages
- super-save
---

After a long period of no development activity[^1],
[super-save](https://github.com/bbatsov/super-save/) gets an update
today!

The latest 0.3 version of your favourite auto-saving library makes it
easier to customize the hook triggers (see `super-save-hook-triggers`)
and adds an option to ignore remote (TRAMP) files (see
`super-save-remote-files`).

As a refresher - `super-save` will save modified files automatically
on certain command (e.g. `switch-to-buffer`) and hook triggers
(e.g. `focus-out-hook`).

Both of those are configurable via `super-save-triggers` and (starting
with 0.3) `super-save-hook-triggers`. Here are a couple of examples:

``` elisp
;; add integration with ace-window
(add-to-list 'super-save-triggers 'ace-window)

;; save on find-file
(add-to-list 'super-save-hook-triggers 'find-file-hook)
```

You can now turn off `super-save` for remote files like this:

``` elisp
(setq super-save-remote-files nil)
```

It seems that now `super-save` is beyond perfect, so don't expect the
next release any time soon!

**P.S.** `super-save` was extracted from
[Prelude](https://github.com/bbatsov/prelude), but for some reason I
actually forgot to add it to Prelude. Today that changes as well! :smile:

[^1]: Mostly because it was perfect from the start.
