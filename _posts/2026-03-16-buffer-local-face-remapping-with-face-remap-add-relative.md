---
layout: post
title: "Buffer-Local Face Remapping with face-remap-add-relative"
date: 2026-03-16 8:30 +0200
tags:
- Emacs 23
- Faces
---

Yet another [neocaml](https://github.com/bbatsov/neocaml) issue taught me
something I didn't know about Emacs. Someone
[requested](https://github.com/bbatsov/neocaml/issues/32) that the docs show
how to customize font-lock faces *per mode* rather than globally. The suggested
approach used `face-remap-add-relative` -- a function I'd never heard of, despite
20+ years of daily Emacs use. Time to dig in.

## The Problem

Let's say you want type names in OCaml buffers to be displayed in a different
color than in, say, Python buffers. The naive approach is to use `custom-set-faces`
or `set-face-attribute`:

``` emacs-lisp
(set-face-attribute 'font-lock-type-face nil :foreground "DarkGreen")
```

This works, but it's a **global** change -- every buffer that uses
`font-lock-type-face` will now show types in dark green.

### When Global is Fine

To be fair, global face customization is perfectly valid in several scenarios:

- **Your theme doesn't cover certain faces.** Many themes only style a subset of
  the faces that various packages define. If a face looks wrong or unstyled, a
  global `set-face-attribute` or `custom-set-faces` is the quickest fix.

- **Your theme makes choices you disagree with.** I recently ran into this with
  the [Catppuccin theme](https://github.com/catppuccin/emacs) -- the styling of
  `font-lock-variable-name-face` didn't match how I expected it to look, so I
  [filed a PR](https://github.com/catppuccin/emacs/pull/212). It got closed
  because different modes interpret that face inconsistently (definitions in
  Elisp, references in C), making a universal fix impractical for the theme.
  That's exactly the kind of situation where you'd want to override the face
  yourself.

- **You want consistent styling everywhere.** If you just want *all* comments
  to be italic, or *all* strings to use a specific color regardless of mode,
  global is the way to go. No need to complicate things with per-buffer
  remapping.

### When Global Isn't Enough

The problem comes when you bounce between several languages (and let's be
honest, most of us do) and you want different visual treatment depending
on the mode. Not all modes use the built-in font-lock faces consistently, and for
some -- especially markup languages -- there's a lot of room for improvisation
in how faces get applied. A global change to `font-lock-keyword-face` might
look great in your Python buffers but terrible in your Org files.

That's where buffer-local face remapping comes in.

## Enter `face-remap-add-relative`

`face-remap-add-relative` has been around since **Emacs 23** (it lives in
`face-remap.el`), and it does exactly what the name suggests -- it remaps a face
*relative* to its current definition, and only in the current buffer. The change
is buffer-local, so it won't leak into other buffers.

Here's the basic usage:

``` emacs-lisp
(face-remap-add-relative 'font-lock-type-face :foreground "DarkGreen")
```

To apply this automatically in a specific mode, hook it up:

``` emacs-lisp
(defun my-ocaml-faces ()
  "Customize faces for OCaml buffers."
  (face-remap-add-relative 'font-lock-type-face :foreground "DarkGreen")
  (face-remap-add-relative 'font-lock-function-name-face :weight 'bold))

(add-hook 'neocaml-mode-hook #'my-ocaml-faces)
```

Now OCaml buffers get their own face tweaks while everything else stays
untouched. You can do the same for any mode -- just swap the hook and
adjust the faces to taste.

## The Magic "Cookie"

`face-remap-add-relative` returns a *cookie* -- a token you'll need if you want
to undo the remapping later. If you're just setting things up in a mode hook
and leaving them, you can ignore the cookie. But if you want to toggle the
remapping on and off, you'll need to hold onto it:

``` emacs-lisp
(defvar-local my-type-face-cookie nil
  "Cookie for type face remapping.")

(defun my-toggle-type-face ()
  "Toggle custom type face in current buffer."
  (interactive)
  (if my-type-face-cookie
      (progn
        (face-remap-remove-relative my-type-face-cookie)
        (setq my-type-face-cookie nil)
        (message "Type face remapping removed"))
    (setq my-type-face-cookie
          (face-remap-add-relative 'font-lock-type-face
                                   :foreground "DarkGreen"))
    (message "Type face remapping applied")))
```

Note the use of `defvar-local` -- since face remapping is buffer-local, your
cookie variable should be too.

A cleaner approach is to wrap this in a minor mode:

``` emacs-lisp
(defvar-local my-type-remap-cookie nil)

(define-minor-mode my-type-remap-mode
  "Minor mode to remap type face in current buffer."
  :lighter " TypeRemap"
  (if my-type-remap-mode
      (setq my-type-remap-cookie
            (face-remap-add-relative 'font-lock-type-face
                                     :foreground "DarkGreen"))
    (when my-type-remap-cookie
      (face-remap-remove-relative my-type-remap-cookie)
      (setq my-type-remap-cookie nil))))
```

## Related Functionality

A few more things worth knowing in this area:

- **`face-remap-set-base`** -- sets the *base* remapping for a face in the current
  buffer. Unlike `face-remap-add-relative` (which layers on top of the existing
  face), this *replaces* the face definition entirely for that buffer. Use this
  when you want to completely override a face rather than tweak it.

- **`buffer-face-mode`** / **`buffer-face-set`** -- a built-in minor mode that
  remaps the `default` face in the current buffer. This is what powers
  `M-x buffer-face-set` and is handy if you want a different base font in
  specific buffers (say, a proportional font for prose and a monospace font
  for code).

- **`text-scale-adjust`** (`C-x C-=` / `C-x C--`) -- the familiar text scaling
  commands actually use `face-remap-add-relative` under the hood to remap the
  `default` face. So if you've ever zoomed text in a single buffer, you've been
  using face remapping without knowing it.

- **`face-remapping-alist`** -- the buffer-local variable where all of this state
  is stored. You generally shouldn't manipulate it directly (that's what the
  functions above are for), but it's useful for debugging -- check its value in
  a buffer to see what remappings are active.

## Wrapping Up

I have to admit -- I'm a bit embarrassed that `face-remap-add-relative` has been
sitting in Emacs since version 23 and I'd never once used it. Probably because
I never felt the need for per-mode face customizations -- but I can certainly see
why others would, especially when working across languages with very different
syntax highlighting conventions.

Working on [neocaml](https://github.com/bbatsov/neocaml) has been a gold mine of
learning (and relearning). I'm happy to keep sharing the things I discover along
the way. Keep hacking!
