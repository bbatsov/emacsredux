---
layout: post
title: "super-save 0.5: Modernized and Better Than Ever"
date: 2026-03-18 11:00 +0200
tags:
- Packages
- super-save
---

It's been a while since the last
[super-save](https://github.com/bbatsov/super-save/) release. The last time I
[wrote about it]({% post_url 2018-09-29-super-save-0-dot-3 %}) was back in 2018,
when I boldly proclaimed:

> It seems that now super-save is beyond perfect, so don't expect the next
> release any time soon!

Famous last words. There *was* a 0.4 release in 2023 (adding a predicate system,
buffer exclusions, silent saving, and trailing whitespace cleanup), but I never
got around to writing about it. The package has been rock solid for years and I
just didn't pay it much attention -- it quietly did its job, which is kind of the
whole point of an auto-save package.

## A Bit of History

The idea behind `super-save` goes all the way back to a [blog post I wrote in
2012](https://batsov.com/articles/2012/03/08/emacs-tip-number-5-save-buffers-automatically-on-buffer-or-window-switch/)
about auto-saving buffers on buffer and window switches. I had been using
IntelliJ IDEA for Java development and loved that it would save your files
automatically whenever the editor lost focus. No manual `C-x C-s`, no thinking
about it. I wanted the same behavior in Emacs.

Back then, the implementation was crude -- `defadvice` on `switch-to-buffer`,
`other-window`, and the `windmove` commands. That code lived in [Emacs
Prelude](https://github.com/bbatsov/prelude) for a few years before I extracted
it into a standalone package in 2015. `super-save` was born.

## What Prompted This Release

Yesterday I stumbled upon
[buffer-guardian.el](https://github.com/jamescherti/buffer-guardian.el), a
package with very similar goals to `super-save`. Its README has a [comparison
with
super-save](https://github.com/jamescherti/buffer-guardian.el?tab=readme-ov-file#how-does-buffer-guardian-compare-with-super-save)
that highlighted some valid points -- mainly that `super-save` was still relying
on advising specific commands for buffer-switch detection, while newer Emacs
hooks like `window-buffer-change-functions` and
`window-selection-change-functions` could do the job more reliably.

The thing is, those hooks didn't exist when `super-save` was created, and I
didn't rush to adopt them while Emacs 27 was still new and I wanted to support
older Emacsen. But it's 2026 now -- Emacs 27.1 is ancient history. Time to
modernize!

## What's New in 0.5

This is the biggest `super-save` release in years! Here are the highlights:

### Modern buffer/window switch detection

Buffer and window switches are now detected via
`window-buffer-change-functions` and `window-selection-change-functions`,
controlled by the new `super-save-when-buffer-switched` option (enabled by
default). This catches *all* buffer switches -- keyboard commands, mouse clicks,
custom functions -- unlike the old approach of advising individual (yet central)
commands.

### Modern focus handling

Frame focus loss is now detected via `after-focus-change-function` instead of the
obsolete `focus-out-hook`, controlled by `super-save-when-focus-lost` (also
enabled by default).

### Soft-deprecated trigger system

With the new hooks in place, both `super-save-triggers` and
`super-save-hook-triggers` now default to `nil`. You can still use them for edge
cases, but for the vast majority of users, the built-in hooks cover everything.

### org-src and edit-indirect support

`super-save` now knows how to save `org-src` edit buffers (via
`org-edit-src-save`) and `edit-indirect` buffers (via `edit-indirect--commit`).
Both are enabled by default and controlled by `super-save-handle-org-src` and
`super-save-handle-edit-indirect`.

### Safer predicates

Two new default predicates prevent data loss: `verify-visited-file-modtime`
avoids overwriting files modified outside Emacs, and a directory existence check
prevents errors when a file's parent directory has been removed. Predicate
evaluation is also wrapped in `condition-case` now, so a broken custom predicate
logs a warning instead of silently disabling all auto-saving.

### Emacs 27.1 required

This allowed cleaning up the code and relying on modern APIs.

## Upgrading

For most users, upgrading is seamless -- the new defaults just work. If you had
a custom `super-save-triggers` list for buffer-switching commands, you can
probably remove it entirely:

``` emacs-lisp
;; Before: manually listing every command that switches buffers
(setq super-save-triggers
      '(switch-to-buffer other-window windmove-up windmove-down
        windmove-left windmove-right next-buffer previous-buffer))

;; After: the window-system hooks catch all of these automatically
;; Just delete the above and use the defaults!
```

If you need to add triggers for commands that *don't* involve a buffer switch
(like `ace-window`), `super-save-triggers` is still available for that.

A clean 0.5 setup looks something like this:

``` emacs-lisp
(use-package super-save
  :ensure t
  :config
  ;; Save buffers automatically when Emacs is idle
  (setq super-save-auto-save-when-idle t)
  ;; Don't display "Wrote file..." messages in the echo area
  (setq super-save-silent t)
  ;; Disable the built-in auto-save (backup files) since super-save handles it
  (setq auto-save-default nil)
  (super-save-mode +1))
```

It's also worth noting that Emacs 26.1 introduced `auto-save-visited-mode`,
which saves file-visiting buffers to their actual files after an idle delay.
This overlaps with `super-save-auto-save-when-idle`, so if you prefer using
the built-in for idle saves, you can combine the two:

``` emacs-lisp
(use-package super-save
  :ensure t
  :config
  ;; Don't display "Wrote file..." messages in the echo area
  (setq super-save-silent t)
  ;; Disable the built-in auto-save (backup files)
  (setq auto-save-default nil)
  (super-save-mode +1))

;; Let the built-in auto-save-visited-mode handle idle saves
(auto-save-visited-mode +1)
```

## Burst-Driven Development Strikes Again

Most of my Emacs packages are a fine example of what I like to call
[burst-driven
development](https://batsov.com/articles/2025/11/16/burst-driven-development-my-approach-to-oss-projects-maintenance/)
-- long periods of stability punctuated by short intense bursts of activity. I
hadn't touched `super-save` in years, then spent a few hours modernizing the
internals, adding a test suite, improving the documentation, and cutting a
release. It was fun to revisit the package after all this time and bring it up to
2026 standards.

If you've been using `super-save`, update to 0.5 and enjoy the improvements. If
you haven't tried it yet -- give it a shot. Your poor fingers might thanks for you this,
as pressing `C-x C-s` non-stop is hard work!

That's all I have for you today. Keep hacking!
