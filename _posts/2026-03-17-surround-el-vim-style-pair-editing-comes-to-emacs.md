---
layout: post
title: "surround.el: Vim-Style Pair Editing Comes to Emacs"
date: 2026-03-17 10:00 +0200
tags:
- Editing
- Packages
---

In my [recent article on removing paired delimiters]({% post_url
2026-03-14-removing-paired-delimiters-in-emacs %}), I mentioned that I kind of
miss Vim's [surround.vim](https://github.com/tpope/vim-surround) experience in
Emacs. Well, it turns out someone has done something about it --
[surround.el](https://github.com/mkleehammer/surround) brings the core ideas of
`surround.vim` to native Emacs, without requiring Evil mode.

`surround.vim` (and similar plugins like
[mini.surround](https://github.com/echasnovski/mini.surround) in Neovim) are
some of my favorite Vim packages. The idea is so simple and so useful that it
feels like it should be a built-in. So I'm happy to see someone took the time to
port that beautiful idea to Emacs.

## The Core Ideas of surround.vim

For those who haven't used `surround.vim`, the concept is straightforward. You
have a small set of operations for working with *surrounding* characters --
the delimiters that wrap some piece of text:

- **Delete** surrounding pair: `ds(` removes the parentheses around point
- **Change** surrounding pair: `cs("` changes parens to quotes
- **Add** surrounding pair: `ys` + motion + character wraps text with a delimiter

That's basically it. Three operations, consistent keybindings, works everywhere
regardless of file type. The beauty is in the uniformity -- you don't need
different commands for different delimiters, and you don't need to think about
which mode you're in.

## surround.el in Practice

`surround.el` is available on MELPA and the setup is minimal:

``` emacs-lisp
(use-package surround
  :ensure t
  :bind-keymap ("M-'" . surround-keymap))
```

You bind a single keystroke (`M-'` in this example) to `surround-keymap`, and that
gives you access to all the operations through a second keystroke. Here are the
commands available in the keymap:

| Key   | Operation                |
|-------|--------------------------|
| `s`   | Surround region/symbol at point  |
| `d`   | Delete surrounding pair  |
| `c`   | Change pair to another   |
| `k`   | Kill text inside pair    |
| `K`   | Kill text including pair |
| `i`   | Mark (select) inside pair|
| `o`   | Mark including pair      |

There are also shortcuts for individual characters -- pressing an opening
delimiter (like `(`) after the prefix does a `mark-inner`, while pressing the
closing delimiter (like `)`) does a `mark-outer`.

### A Short Walkthrough

Let's see how this works in practice. Starting with the word `Hello` (with point
somewhere on it):

**Surround** -- `M-' s "` wraps the symbol at point with quotes:

```
Hello  →  "Hello"
```

**Change** -- `M-' c " (` changes the surrounding quotes to parens:

```
"Hello"  →  (Hello)
```

**Mark inner** -- `M-' i (` selects just the text inside the parens:

```
(|Hello|)    ;; "Hello" is selected
```

**Mark outer** -- `M-' o (` (or `M-' )`) selects the parens too:

```
|( Hello)|   ;; "(Hello)" is selected
```

**Delete** -- `M-' d (` removes the surrounding parens:

```
(Hello)  →  Hello
```

**Kill** -- `M-' k (` kills the text inside the pair (leaving the delimiters
gone too), while `M-' K (` kills everything including the delimiters.

### Inner vs. Outer

Like `surround.vim`, `surround.el` distinguishes between "inner" (just the
content between delimiters) and "outer" (content plus the delimiters themselves).
The `i` and `k` commands operate on inner text, `o` and `K` on outer.

There's also an "auto" mode -- the default for `i` and `k` -- that behaves as
inner when you type an opening character and outer when you type a closing
character. So `M-' (` marks inner, `M-' )` marks outer. Handy shortcut if your
fingers remember it (I'm still building the muscle memory).

One caveat: auto mode can't distinguish inner from outer for symmetric pairs
like quotes (`"`, `'`), since the opening and closing character are the same. In
those cases it defaults to inner.

## How It Differs from Vim

The biggest difference is in how you *surround* text. In Vim, `surround.vim`
uses motions -- `ysiw(` means "surround inner word with parens." In Emacs,
`surround.el` operates on the active region or the symbol at point. So the
typical workflow is: select something, then `M-' s (`.

This actually pairs beautifully with
[expreg](https://github.com/casouri/expreg) (which I [wrote about
recently]({% post_url 2026-03-03-expreg-expand-region-reborn %})). Use `expreg`
to incrementally select exactly the text you want, then `M-' s` to wrap it. It's
a different rhythm than Vim's motion-based approach, but once you get used to
it, it feels natural.

The other operations (`d`, `c`, `k`, `i`, `o`) work similarly to their Vim
counterparts -- you invoke the command and then specify which delimiter you're
targeting.

## When to Use It

`surround.el` fills a specific niche:

- **Using `electric-pair-mode`?** Then `surround.el` is an excellent complement.
  `electric-pair-mode` handles auto-pairing when you *type* delimiters, but
  offers nothing for removing, changing, or wrapping existing text with
  delimiters. `surround.el` fills exactly that gap.

- **Using `smartparens`?** You probably don't need `surround.el` --
  `smartparens` already has `sp-unwrap-sexp`, `sp-rewrap-sexp`, and friends. The
  overlap is significant, and adding another package on top would just be
  confusing.

- **Using `paredit`?** Same story for Lisp code -- `paredit` has you covered
  with `paredit-splice-sexp`, `paredit-wrap-round`, and so on. But if you want
  the surround experience in non-Lisp buffers, `surround.el` is a good pick.

My current setup is `paredit` for Lisps, `electric-pair-mode` for everything
else, and I'm adding `surround.el` to complement the latter. Early days, but it
feels right.

## Wrapping Up

If you've ever wondered whether some Vim feature you miss exists in Emacs --
the answer is always yes. It's Emacs. Of course someone has written a package
for it. Probably several, in fact... Admittedly, I discovered `surround.el` only
when I had decided to port `surround.vim` to Emacs myself. :D

That's all I have for you today. Keep hacking!
