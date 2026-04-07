---
layout: post
title: "Stealing from the Best Emacs Configs"
date: 2026-04-07 09:00 +0300
tags:
- Configuration
- Productivity
---

> Good artists borrow, great artists steal.
>
> -- Pablo Picasso

After spending the past couple of weeks updating
[Prelude](https://github.com/bbatsov/prelude) and my [personal Emacs
config](https://github.com/bbatsov/emacs.d), I figured it wouldn't
hurt to see what the competition has been up to. I hadn't done a
proper survey of other people's configs in years, and the Emacs
landscape has changed quite a bit since the last time I looked.

So I went through [Doom Emacs](https://github.com/doomemacs/doomemacs),
[Purcell's emacs.d](https://github.com/purcell/emacs.d), [Centaur
Emacs](https://github.com/seagle0128/.emacs.d), [Prot's
dotfiles](https://github.com/protesilaos/dotfiles), and a handful of
others. Here are some of the most interesting things I found -- settings
and tricks that I either didn't know about or had forgotten about
entirely.

<!--more-->

## Performance Tweaks

### Disable Bidirectional Text Scanning (Doom Emacs)

If you don't edit right-to-left languages (Arabic, Hebrew, etc.),
Emacs is doing a bunch of work on every redisplay cycle for nothing.
These settings tell Emacs to assume left-to-right text everywhere and
skip the bidirectional parenthesis algorithm:

``` emacs-lisp
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
```

The difference is hard to measure in small buffers, but in large files
(think multi-thousand-line JSON or log files) it adds up. Doom enables
this unconditionally and I've never seen anyone complain about it.

### Skip Fontification During Input (Doom Emacs)

Emacs normally fontifies (syntax-highlights) text even while you're
actively typing. This can cause micro-stutters, especially in
tree-sitter modes or large buffers. One setting fixes it:

``` emacs-lisp
(setq redisplay-skip-fontification-on-input t)
```

Emacs will defer fontification until you stop typing. In practice you
never notice the delay -- the highlighting catches up instantly -- but
scrolling and typing may feel smoother.

### Increase Process Output Buffer for LSP (Doom, Purcell, Centaur)

The default `read-process-output-max` is 64KB, which is still quite
conservative. Modern LSP servers like `rust-analyzer` or `clangd` routinely
send multi-megabyte responses. Bumping this reduces the number of
read calls Emacs has to make:

``` emacs-lisp
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
```

If you use eglot (or lsp-mode), this is basically free performance.
Three of the most popular configs out there all set it -- that should
tell you something.

**Note:** I'm really surprised I didn't discover this one sooner.
Probably that's because I rarely work on big projects these days.

### Don't Render Cursors in Non-Focused Windows (Doom Emacs)

If you have several windows visible, Emacs draws a cursor in each of
them -- even the ones you're not working in. It also highlights
selections in non-focused windows. Two settings to stop that:

``` emacs-lisp
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
```

This is mostly a visual preference (I don't mind the phantom cursors
but I know some people find them distracting), but it also reduces rendering work.

All four of these performance settings are safe to add unconditionally
-- they have no downsides for the vast majority of users.

## Kill Ring (Emacs's Clipboard History) and Clipboard

### Save the Clipboard Before Killing (Purcell, Prot, Centaur)

Here's a scenario: you copy a URL from your browser, switch to Emacs,
kill a line with `C-k`, and then try to yank the URL you copied earlier with `C-y`.
Gone. The kill replaced it on the clipboard.

This setting makes Emacs save the existing clipboard content into the
kill ring *before* overwriting it:

``` emacs-lisp
(setq save-interprogram-paste-before-kill t)
```

Now `C-y` gets the kill, and `M-y` gets you back to the URL. Such a
small thing, but it eliminates a genuinely annoying problem.

### No Duplicates in the Kill Ring (Doom, Prot)

Kill the same line three times and you get three identical entries in
the kill ring, wasting slots. This deduplicates them:

``` emacs-lisp
(setq kill-do-not-save-duplicates t)
```

### Persist the Kill Ring Across Sessions (Doom, Prot)

Most configs that use `savehist-mode` only persist search rings. But
`savehist` can save any variable -- including the kill ring. Add it
and you get clipboard history that survives restarts:

``` emacs-lisp
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))
```

One thing to watch out for: the kill ring can accumulate text
properties (fonts, overlays, etc.) that bloat the savehist file. Doom
strips them before saving:

``` emacs-lisp
(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (cl-remove-if-not #'stringp kill-ring)))))
```

Probably overkill for most people, but it's good to be aware of if
your savehist file starts growing suspiciously large.

## Editing

### Auto-Chmod Scripts on Save (Multiple Configs)

If you create a file that starts with `#!` (a shebang line), it should
be executable. But you always forget to `chmod +x` it, run the script,
get "Permission denied", curse, go back, chmod, try again. This hook
does it automatically:

``` emacs-lisp
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)
```

Save a file with a shebang, and Emacs `chmod +x`es it for you. One
of those things that should arguably be a default.

**Note:** Okay, I have to admit I've always known this one, but seeing
it in so many configs made me want to include it here.

### Sane Syntax in re-builder (Multiple Configs)

`re-builder` (`M-x re-builder`) is an interactive tool for developing
regexps -- you type a pattern and see matches highlighted live in the
target buffer. The problem is the default syntax: `read`. In read
syntax, you have to double-escape everything, so a word boundary is
`\\<` and a group is `\\(...\\)`. It's the regexp equivalent of
trying to type with oven mitts on.

Switch to string syntax and things look like normal Emacs regexps:

``` emacs-lisp
(setq reb-re-syntax 'string)
```

Now `\<` is `\<` and `\(foo\)` is `\(foo\)`. Much less painful.

**See also:** If you want live feedback on the regexp *structure* as
you type it (color-coded groups, character classes, etc.), check out
[minibuffer-regexp-mode]({% post_url 2026-04-06-minibuffer-regexp-mode %})
-- a new built-in mode in Emacs 30.

### Prevent ffap from Pinging Hostnames (Centaur Emacs)

Ever had Emacs freeze for a few seconds when you ran
`find-file-at-point` (or a command that uses it internally)? If the
text under point looks like a hostname -- say, `something.com` in a
comment -- ffap tries to ping it to check if it's reachable. On a
slow or firewalled network, that's a multi-second hang.

``` emacs-lisp
(setq ffap-machine-p-known 'reject)
```

This tells ffap to never try network lookups. If you actually want to
open a remote file, you can type the path explicitly.

## Windows

### Proportional Window Resizing (Purcell, Prot)

When you split a window with `C-x 2` or `C-x 3`, Emacs halves the
current window. If you already have a multi-window layout, this can
produce one awkwardly tiny window while others stay large. With this
setting, *all* windows in the frame resize proportionally:

``` emacs-lisp
(setq window-combination-resize t)
```

The difference is subtle but makes multi-window layouts feel more
balanced without manual resizing.

### Reversible `C-x 1` (Purcell)

`C-x 1` (`delete-other-windows`) is the nuclear option -- it nukes
your entire window layout to focus on one buffer. Then you spend the
next minute recreating the layout you just destroyed.

With `winner-mode` and a small wrapper, you can make `C-x 1`
toggle: press it once to go single-window, press it again to restore
the previous layout:

``` emacs-lisp
(winner-mode +1)

(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)
```

Just drop this into your config as-is -- it's self-contained. This is
one of those tricks where once you have it, you can't imagine going
back.

## Misc

### Faster Mark Popping (Purcell, Centaur, Prot)

The mark ring is one of Emacs's most underused navigation features.
Every time you jump somewhere -- `isearch`, `M-<`, `M->`,
`goto-line`, `imenu`, and many more -- Emacs pushes your old position
onto the mark ring. `C-u C-SPC` pops it, jumping you back.

The annoyance: you need `C-u C-SPC` every single time. With this
setting, after the first `C-u C-SPC` you can keep pressing just
`C-SPC` to continue popping:

``` emacs-lisp
(setq set-mark-command-repeat-pop t)
```

This pairs beautifully with `repeat-mode` if you have it enabled
(and you should -- see [my earlier post on repeat-mode]({% post_url
2026-04-04-repeat-mode %})).

### Recenter After save-place Restores Position (Doom Emacs)

`save-place-mode` is great -- it remembers where you were in each file
and jumps back there when you reopen it. The problem is that it can
leave your cursor on the last visible line of the window, which is
disorienting. This advice recenters the view after the jump:

``` emacs-lisp
(advice-add 'save-place-find-file-hook :after
            (lambda (&rest _)
              (when buffer-file-name (ignore-errors (recenter)))))
```

Small thing, but it makes reopening files feel much more natural.

### Auto-Select Help Windows (Prot)

When you press `C-h f` or `C-h v`, Emacs opens the help buffer but
leaves your cursor in the original window. You almost always want to
read the help right away, so you end up pressing `C-x o` every
single time. This fixes it:

``` emacs-lisp
(setq help-window-select t)
```

**Bonus:** Many of the configs I surveyed also use built-in lazy
isearch counting (showing "match N of M" in the minibuffer) instead
of third-party packages like `anzu`. I recently wrote about that in
[a dedicated post]({% post_url 2026-03-15-isearch-lazy-count %}).

---

The funny thing about all of this is how much overlap there is between
configs. Half of these tricks appear in three or four of the configs I
surveyed. At this point I'm convinced there are about 200 essential
Emacs settings floating around in the collective unconscious, and
every serious config independently converges on roughly the same
subset. Picasso was right -- we all steal from each other, and the
kill ring makes it embarrassingly easy. `M-w` and move on.

That's all I have for you today! Keep hacking!
