---
layout: post
title: "Cape: Corfu's Best Friend"
date: 2026-07-13 09:00 +0300
tags:
- Packages
- Completion
---

I've been using [corfu](https://github.com/minad/corfu) for in-buffer
completion for a while now and I'm quite happy with it. There was one thing
that kept bugging me, though - the completion popup would only ever show
whatever the current major mode's completion function had to offer. No plain
word completion, no file name completion - if the major mode (or your LSP
server) didn't know about something, neither did the popup.

Turns out I was simply missing one piece of the puzzle, namely
[cape](https://github.com/minad/cape).

<!--more-->

## The Problem

A bit of background first. Modern Emacs completion is built around
`completion-at-point-functions` (capfs for short) - each major mode registers a
function that knows how to complete things in its buffers, and UIs like
`corfu` (or the built-in `completion-at-point`) simply display the results.

The catch is that most major modes register *only* their own capf. That's why
the trusty old `dabbrev`-style completion (complete any word that appears in
your buffers) is nowhere to be found in the popup, even though most of us have
been relying on it for decades via `M-/`.

## Enter Cape

Cape (**C**ompletion **A**t **P**oint **E**xtensions) is another package by the
prolific Daniel Mendler and it does exactly what its name suggests - it
provides a bunch of extra capfs that you can mix into any buffer:

- `cape-dabbrev` - complete words from the current and other buffers (the star
  of the show, in my opinion)
- `cape-file` - complete file paths
- `cape-keyword` - complete programming language keywords
- `cape-elisp-symbol` - complete Elisp symbols anywhere (e.g. in comments or docstrings)
- `cape-dict` - complete words from a dictionary file
- `cape-line` - complete entire lines from the buffer
- `cape-emoji` - complete emoji, if that's your thing (Emacs 29+)

My setup is deliberately minimal - buffer words and file names everywhere, on
top of whatever the major mode provides:

``` emacs-lisp
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))
```

That's it. Now when I type a few characters, `corfu` shows the major mode's
candidates *and* falls back to buffer words when there's nothing smarter to
offer. Typing a path like `~/proj` offers to complete it as a file name. It's
one of those small quality of life improvements you stop noticing after a day,
because it feels like it has always been there.

**Note:** Every cape capf is also an interactive command, so you can invoke
them on demand - e.g. `M-x cape-file` to complete a file name regardless of
your setup. Cape's README suggests putting them on the `C-c p` prefix, but for
me (and my fellow [Projectile](https://github.com/bbatsov/projectile) users)
that's a no-go, as `C-c p` is Projectile territory. Pick your own prefix if
you want quick access to them.

## Tips and Tricks

A few practical things worth knowing:

- Type `file:` anywhere and `cape-file` kicks in for the text right after it,
  even in places where a path wouldn't normally be recognized (say, in the
  middle of a comment). The prefix is customizable via `cape-file-prefix`.
- `cape-dabbrev` gathers its candidates from buffers with the same major mode
  by default, so your Elisp buffers won't pollute the completions of your
  Clojure buffers. If you want different behavior, that's
  `cape-dabbrev-buffer-function`.
- If you set `tab-always-indent` to `complete` (as I do), `TAB` becomes the
  perfect trigger for the whole completion stack - it indents the line if
  needed and otherwise summons `corfu` with all your capfs, cape ones included:

``` emacs-lisp
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)
```

- `cape-dict` reads from `/usr/share/dict/words` by default, which is fine on
  macOS and most Linux distros, but you can point `cape-dict-file` to any word
  list - e.g. one for your native language.

## A Couple of Power Tricks

Cape also ships a few combinators that are worth knowing about:

- `cape-capf-super` merges several capfs into one, so their candidates show up
  in a single unified popup. (e.g. you can blend `eglot`'s completion with
  `cape-dabbrev`)
- `cape-company-to-capf` converts `company-mode` backends into capfs, which is
  super handy if there's a company backend for your favorite tool but no capf
  in sight.

I haven't needed either of them yet, but it's good to know they are there.

## Closing Thoughts

If you're already on the `vertico`/`consult`/`corfu` bandwagon, `cape` is
pretty much a mandatory addition - `corfu` deliberately stays small and
focused, and `cape` is the intended way to extend what it can complete.[^1]
Funny enough, I went years without it simply because I never stopped to ask
why file names wouldn't complete in the popup. Sometimes you don't know
something is missing until you try it.

What's in your `completion-at-point-functions`? Any cape goodies I've
overlooked? Let me know in the comments!

That's all I have for you today. Keep completing (at point)!

[^1]: Also, let's be honest - corfu wearing a cape is a pretty great mental image.
