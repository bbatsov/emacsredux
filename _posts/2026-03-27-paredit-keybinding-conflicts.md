---
layout: post
title: "Paredit's Keybinding Conflicts"
date: 2026-03-27 10:00 +0200
tags:
- Paredit
- smartparens
---

Today's topic came up while I was going over the list of open Prelude issues
after doing the recent 2.0 release.

[Paredit](https://paredit.org) and
[smartparens](https://github.com/Fuco1/smartparens) are structural
editing packages that keep your parentheses balanced and let you
manipulate s-expressions as units -- essential tools for anyone writing
Lisp. Paredit has been around since 2005 and its keybindings have
become muscle memory for a generation of Lisp programmers (yours truly
included). Smartparens inherits the same keymap when used with
`sp-use-paredit-bindings`.

The problem is that some of those keybindings conflict with standard
Emacs key prefixes that didn't exist when paredit was written -- or
that have grown more important over time.

<!--more-->

## The Commands and Their Conflicts

Before getting to solutions, let's look at each problematic command --
what it does, where paredit puts it, and what it shadows.

### Splice -- `M-s`

`paredit-splice-sexp` (or `sp-splice-sexp` in smartparens) removes
the enclosing delimiters around point, "splicing" the contents into
the parent expression:

```clojure
;; before (point on "b"):
(a (b c) d)

;; after splice:
(a b c d)
```

The conflict: Emacs uses `M-s` as the `search-map` prefix (since
Emacs 23). Paredit's splice binding shadows `M-s o` (`occur`), `M-s .`
(`isearch-forward-symbol-at-point`), and any `M-s`-prefixed bindings
from packages like consult (`consult-line`, `consult-ripgrep`, etc.).
If you use a completion framework like Vertico + Consult, this one
really hurts.

### Convolute -- `M-?`

`paredit-convolute-sexp` (or `sp-convolute-sexp`) swaps the nesting
of two enclosing forms. Specifically, it takes the head of the outer
form and moves it inside the inner one:

```clojure
;; before (point on "c"):
(a (b c d))

;; after convolute -- "a" moved from outer to inner:
(b (a c d))
```

The conflict: Emacs uses `M-?` for `xref-find-references` (since
Emacs 25). If you use LSP (Eglot or lsp-mode), paredit's convolute
binding shadows "find all references" -- one of the most useful LSP
features.

### Slurp -- `C-<right>`

`paredit-forward-slurp-sexp` (or `sp-forward-slurp-sexp`) expands
the current sexp forward by pulling the next sibling inside the
closing delimiter:

```clojure
;; before:
(a b) c

;; after slurp -- "c" pulled inside:
(a b c)
```

### Barf -- `C-<left>`

`paredit-forward-barf-sexp` (or `sp-forward-barf-sexp`) is the
opposite -- it pushes the last element out past the closing delimiter:

```clojure
;; before:
(a b c)

;; after barf -- "c" pushed out:
(a b) c
```

The conflict for both: `C-<right>` and `C-<left>` override
`right-word` and `left-word`. Fine if you're in a Lisp buffer and
know what you're doing, but surprising if you expected word-level
movement.

### Splice-killing-backward -- `M-<up>`

`paredit-splice-sexp-killing-backward` splices (removes delimiters)
and also kills everything *before* point within the sexp:

```clojure
;; before (point on "c"):
(a b c d)

;; after splice-killing-backward -- "a b" killed, parens removed:
c d
```

### Splice-killing-forward -- `M-<down>`

`paredit-splice-sexp-killing-forward` does the same but kills
everything *after* point:

```clojure
;; before (point on "b"):
(a b c d)

;; after splice-killing-forward -- "c d" killed, parens removed:
a b
```

The conflict for both: `M-<up>` and `M-<down>` clash with
`org-metaup`/`org-metadown` in Org mode, paragraph movement in some
configs, and window manager shortcuts on some Linux desktops.

## What to Do About It

The good news is that both Matus Goljer (a.k.a. Fuco1, the smartparens
author) and Magnar Sveen (a.k.a. Magnars, the author of
`expand-region`, `multiple-cursors` and many other popular packages)
have solved these conflicts in their own configs. Their approaches are
worth borrowing.

The examples below use smartparens. For paredit, replace
`smartparens-mode-map` with `paredit-mode-map` and `sp-*` commands
with their `paredit-*` equivalents.

### Splice (`M-s`)

Matus's approach is to rebind to `M-D` (meta-shift-d). The mnemonic
is nice -- `M-d` kills a word, `M-D` "kills the delimiters." This is
probably the most widely copied alternative:

```emacs-lisp
(define-key smartparens-mode-map (kbd "M-s") nil)
(define-key smartparens-mode-map (kbd "M-D") #'sp-splice-sexp)
```

Magnar's approach is to rebind to `s-s` (super-s). Clean if you're on
macOS where Super is the Command key:

```emacs-lisp
(define-key smartparens-mode-map (kbd "M-s") nil)
(define-key smartparens-mode-map (kbd "s-s") #'sp-splice-sexp)
```

You can use both -- `M-D` everywhere, `s-s` as a macOS bonus.

### Convolute (`M-?`)

Convolute-sexp is one of paredit's more obscure commands. If you use
LSP or xref regularly, freeing `M-?` for `xref-find-references` is
a net win:

```emacs-lisp
(define-key smartparens-mode-map (kbd "M-?") nil)
```

If you actually use convolute-sexp, rebind it to something under
a less contested prefix.

### Slurp/barf (`C-<arrow>`)

Magnar moves these to Super:

```emacs-lisp
(define-key smartparens-mode-map (kbd "C-<right>") nil)
(define-key smartparens-mode-map (kbd "C-<left>") nil)
(define-key smartparens-mode-map (kbd "s-<right>") #'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "s-<left>") #'sp-forward-barf-sexp)
```

Matus keeps the `C-<arrow>` bindings (accepting the conflict). This
one's really a matter of taste -- if word-level movement with
`C-<arrow>` matters to you, move them. If you're a Lisp programmer
who slurps more than they word-move, keep them.

### Splice-killing (`M-<up>` / `M-<down>`)

Matus uses `C-M-<backspace>` and `C-M-<delete>`. Magnar uses
`s-<up>` and `s-<down>`. Both work well.

## The Smartparens Alternative

If you're using smartparens (rather than paredit), there's actually a
simpler option -- just use smartparens' own default keybinding set
instead of the paredit compatibility bindings. Set
`sp-base-key-bindings` to `'sp` (or just don't set it at all) and
call `sp-use-smartparens-bindings` instead of
`sp-use-paredit-bindings`.

The default smartparens bindings already avoid most of the conflicts
above:

| Command | Paredit binding | Smartparens binding |
|---------|----------------|---------------------|
| splice | `M-s` | `M-D` |
| convolute | `M-?` | (unbound) |
| slurp | `C-<right>` | `C-<right>` |
| barf | `C-<left>` | `C-<left>` |
| splice-killing-backward | `M-<up>` | `C-M-<backspace>` |
| splice-killing-forward | `M-<down>` | `C-M-<delete>` |

The two big wins are splice moving to `M-D` (freeing `search-map`)
and convolute not being bound at all (freeing `xref-find-references`).
The slurp/barf conflict with word movement remains, but that's a
trade-off most Lisp programmers are happy to make.

## What about me?

I don't use most of the commands shadowed by Paredit, so I didn't
even think about the conflicts much before today. Given that I'm a
macOS user these days I like Magnar's approach to solving the
conflicts. But I'm also sooo used to pressing `M-s`... Decisions,
decisions...

I definitely think everyone should free up `M-?`, given the default is quite
important command. For me this was never much of a problem in the past (until
the LSP era) as I've always used Projectile's wrappers around `xref` commands --
`projectile-find-references` (`s-p ?` or `C-c p ?`) instead of
`xref-find-references`, and `projectile-find-tag` (`s-p j` or `C-c p j`) instead
of `xref-find-definitions`. Projectile scopes these to the current project
automatically, which is what I usually want anyway.

I don't really care about any commands with arrows in them, as I'm
using an HHKB keyboard and it's not really fun to press arrows on
it...

## The Bottom Line

Paredit's defaults made perfect sense in 2005. Twenty years later,
Emacs has grown `search-map`, xref, and a whole ecosystem of packages
that expect those keys to be available. If you've been living with
these conflicts out of habit, take five minutes to rebind -- your
future self will thank you.

That's all I have for you today. Keep hacking!
