---
layout: post
title: "Repeat Mode: Stop Repeating Yourself"
date: 2026-04-04 10:00 +0300
tags:
- Emacs 28
- Keybindings
---

I've been going through the Emacs 28-30 changelogs recently as part of a big
update to [Prelude](https://github.com/bbatsov/prelude) and my [personal config](https://github.com/bbatsov/emacs.d), looking
for features I never got around to trying. `repeat-mode` is one I wish I'd adopted sooner.

How many times have you typed `C-x o C-x o C-x o` to cycle through a
few windows?  Or `C-x { C-x { C-x {` to keep shrinking one? All that
prefix repetition is pure friction.

`repeat-mode` is a built-in minor mode (Emacs 28+) that lets you
drop the prefix after the first invocation and just keep pressing the
final key. Enable it with one line:

``` emacs-lisp
(repeat-mode 1)
```

<!--more-->

One important thing to understand -- this doesn't magically work for
every key sequence. A command is only "repeatable" if it has been
explicitly added to a repeat map. Emacs ships with repeat maps for a
bunch of common built-in commands, though, so you get a decent
experience out of the box. Here are some of the highlights:

- `C-x o o o` -- keep cycling windows
- `C-x { { {` / `C-x } } }` -- shrink/grow window horizontally
- `C-x ^ ^ ^` -- grow window vertically
- `C-x u u u` -- keep undoing
- `C-x <left> <left>` / `C-x <right> <right>` -- cycle through buffer history
- `M-g n n n` / `M-g p p p` -- jump through next-error results

The transient state ends as soon as you press any key that isn't part
of the repeat map.

If you'd prefer it to time out automatically, there's a setting for that:

``` emacs-lisp
(setq repeat-exit-timeout 5) ;; exit after 5 seconds of inactivity
```

## Defining Your Own Repeat Maps

The real power comes from defining repeat maps for your own commands.
For instance, if you use [expreg](https://github.com/casouri/expreg)
for expand-region, you can set things up so that `C-= = = = -`
expands three times then contracts once:

``` emacs-lisp
(defvar expreg-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "=" #'expreg-expand)
    (define-key map "-" #'expreg-contract)
    map))

(put 'expreg-expand 'repeat-map 'expreg-repeat-map)
(put 'expreg-contract 'repeat-map 'expreg-repeat-map)
```

The pattern is simple: create a keymap, then attach it to the relevant
commands via the `repeat-map` symbol property. Any command with that
property becomes "repeatable" after first invocation.

That's all there is to it. One line to enable, and a lot less `C-x`
mashing in your future.

Are you using `repeat-mode`? Have you defined any custom repeat maps
that you find particularly useful? I'd love to hear about them in the
comments!

Keep hacking!
