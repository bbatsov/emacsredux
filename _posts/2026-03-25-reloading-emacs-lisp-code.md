---
layout: post
title: "Reloading Emacs Lisp Code"
date: 2026-03-25 10:30 +0200
tags:
- Emacs Lisp
---

While working on [erlang-ts-mode](https://github.com/erlang/emacs-erlang-ts/)
recently, someone asked me how I reload the mode's code while developing it. I
realized that while the answer is obvious to me after years of Emacs Lisp
hacking, it's not obvious at all to people who are just getting started with
Emacs Lisp development. So here's a short practical guide.

<!--more-->

## The Problem

Most Emacs users learn early on that you can evaluate Emacs Lisp with commands
like `eval-buffer` (`M-x eval-buffer`), `eval-defun` (`C-M-x`), and
`eval-expression` (`M-:`). These work great for most code -- but they have a
blind spot: `defvar` and `defcustom`.

By design, `defvar` only sets a variable if it's not already bound. This means
that if you change the default value of a `defvar` in your source and then run
`eval-buffer`, the old value sticks around. The same applies to `defcustom`.
Here's a quick example:

``` emacs-lisp
(defvar my-mode-default-indent 2)  ;; eval-buffer sets this to 2

;; Now change it to 4 in source:
(defvar my-mode-default-indent 4)  ;; eval-buffer does NOTHING -- still 2
```

This is intentional -- loading a library shouldn't clobber user
customizations. But when you're *developing* a package, it's a real pain. You
change a default, re-evaluate, and wonder why nothing happened.

The same issue applies to faces defined with `defface` -- re-evaluating the
definition won't update an already-defined face.

## The Approaches

Here are all the approaches I know of, roughly ordered from lightest to heaviest.

### 1. `eval-defun` on Individual Forms

Here's something that surprises many people: `eval-defun` (`C-M-x`) **does**
handle `defvar`, `defcustom`, and `defface` specially. When you place point
inside a `defvar` form and hit `C-M-x`, it unconditionally sets the variable to
the new value, ignoring the "only if unbound" semantics.

This is different from `eval-buffer` and `eval-region`, which respect the
normal `defvar` behavior.

So if you've only changed a few forms, `C-M-x` on each one is the fastest
approach. It's what I use most of the time during development.

### 2. `setq` via `eval-expression`

If you just need to reset one variable quickly, hit `M-:` and type:

``` emacs-lisp
(setq my-mode-default-indent 4)
```

Quick and dirty, but it works. This won't re-evaluate any other code, so it's
only useful for tweaking individual values.

### 3. `load-file`

`M-x load-file` lets you load an `.el` file from disk. The difference from
`require` is that `require` skips loading entirely if the feature is already in
the `features` list, while `load-file` always reads and evaluates the file. That
said, it still respects `defvar` semantics, so already-bound variables won't be
updated -- you'd need `eval-defun` or `unload-feature` for that.

Where `load-file` really helps is when you want to reload a file that isn't the
one you're currently editing -- e.g., a dependency within your package, or a
file that doesn't have a `provide` form.

### 4. `unload-feature` + `require`

This is the "clean reload" approach:

``` emacs-lisp
(unload-feature 'my-mode t)  ;; the t means "force, even if other things depend on it"
(require 'my-mode)
```

`unload-feature` removes everything the feature defined -- variables, functions,
hooks, etc. Then `require` loads it fresh. This is the closest thing to a
clean slate without restarting Emacs.

A few caveats:

- `unload-feature` can be disruptive. If the feature added hooks or advice,
  unloading should clean those up, but edge cases exist.
- It only works for features that were loaded via `provide`/`require`. If you
  loaded a file with `load-file`, there's no feature to unload.
- Some complex packages don't survive `unload-feature` cleanly. For most
  packages (like a typical major mode), it works well.

You can bind this to a key for quick access during development:

``` emacs-lisp
(defun my-reload-feature (feature)
  "Unload and reload FEATURE."
  (interactive
   (list (intern (completing-read "Reload feature: "
                                  features nil t))))
  (unload-feature feature t)
  (require feature))
```

### 5. Restart Emacs

The nuclear option. When nothing else works or when you've made lots of changes
and don't trust the runtime state, just restart. With `desktop-save-mode` or a
session manager, this is less painful than it sounds.

If you use Emacs in daemon mode, `M-x restart-emacs` (from the
[restart-emacs](https://github.com/iqbalansari/restart-emacs) package) or
the built-in `restart-emacs` (Emacs 29+) makes this quick.

## Don't Forget to Re-activate the Mode

One thing that trips people up: after reloading the code (via any of the above
methods), you also need to re-activate the mode in existing buffers. Just run
`M-x my-mode` -- this re-runs the mode function and re-applies keymaps, hooks,
font-lock settings, etc. Without this step, existing buffers will still be
running the old code.

For minor modes, toggle off and on: `M-x my-minor-mode` twice.

## My Workflow

For what it's worth, here's what my typical mode development workflow looks
like:

1. Edit the source file.
2. `C-M-x` on the specific forms I changed.
3. Switch to a test buffer and re-activate the mode with `M-x my-mode`.
4. If things are weird, `unload-feature` + `require` for a clean reload.
5. Restart Emacs only when I've changed something fundamental (like autoloads or
   package metadata).

Most of the time, steps 1-3 are all I need.

## Wrapping Up

The thing to remember is that `defvar`/`defcustom`/`defface` are intentionally
designed not to override existing values, and most evaluation commands respect
this. Once you know that `eval-defun` is the exception (it *does* force the
update) and that `unload-feature` gives you a clean slate, reloading code during
development is pretty simple.

That's all I have for you today. Keep hacking!
