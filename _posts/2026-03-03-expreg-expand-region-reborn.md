---
layout: post
title: "expreg: Expand Region, Reborn"
date: 2026-03-03 16:00 +0200
tags:
- Editing
- Tree-sitter
- Packages
---

[expand-region](https://github.com/magnars/expand-region.el) is one of my all
time favorite Emacs packages. I've been using it since forever -- press a key,
the selection grows to the next semantic unit, press again, it grows further.
Simple, useful, and satisfying. I've mentioned it
quite a few times over the years, and it's been a
permanent fixture in my config for as long as I can remember.

But lately I've been wondering if there's a better way. I've been playing with
Neovim and Helix from time to time (heresy, I know), and both have structural
selection baked in via tree-sitter -- select a node, expand to its parent, and so
on. Meanwhile, I've been building and using more tree-sitter major modes in
Emacs (e.g. [clojure-ts-mode](https://github.com/clojure-emacs/clojure-ts-mode) and
[neocaml](https://github.com/bbatsov/neocaml/)), and the contrast started to
bother me. We have this rich AST sitting right there in the buffer, but
expand-region doesn't know about it.

The fundamental limitation is that expand-region relies on hand-written,
mode-specific expansion functions for each language. Someone has to write and
maintain `er/mark-ruby-block`, `er/mark-python-statement`, `er/mark-html-tag`,
and so on. Some languages have great support, others get generic fallbacks. And
when a new language comes along, you're on your own until someone writes the
expansion functions for it.

<!--more-->

## Enter Tree-sitter

You can probably see where this is going. Tree-sitter gives us a complete AST
for any language that has a grammar, and walking up the AST from a point is
_exactly_ what semantic region expansion needs to do. Instead of hand-written
heuristics per language, you just ask tree-sitter: "what's the parent node?"

Both Clojure and OCaml have rich, deeply nested syntax (s-expressions in
Clojure, pattern matching and nested modules in OCaml), and semantic selection
expansion is invaluable when navigating their code. Rolling my own tree-sitter
based solution crossed my mind, but fortunately someone had already done it
better.

## expreg

[expreg](https://github.com/casouri/expreg) (short for "expand region")[^1] is a
package by Yuan Fu -- the same person who implemented Emacs's built-in
tree-sitter support. Yuan Fu created expreg in mid-2023, shortly after the
tree-sitter integration shipped in Emacs 29, and it landed on GNU ELPA soon after.
It's a natural extension of his tree-sitter work: if you've given Emacs a
proper understanding of code structure, you might as well use it for selection
too.

The package requires Emacs 29.1+ and the setup is minimal:

``` emacs-lisp
(use-package expreg
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))
```

That's it. Two commands: `expreg-expand` and `expreg-contract`. If you've used
expand-region, you already know the workflow.

## What Makes It Better Than expand-region

**It works with any tree-sitter grammar automatically.** No per-language
configuration. If your buffer has a tree-sitter parser active, expreg walks the
AST to generate expansion candidates. OCaml, Clojure, Rust, Go, Python, C --
all covered with zero language-specific code.

**It works without tree-sitter too.** This is the key insight that makes expreg a
true expand-region replacement rather than just a tree-sitter toy. When no
tree-sitter parser is available, it falls back to Emacs's built-in syntax tables
for words, lists, strings, comments, and defuns. So it works in
`fundamental-mode`, `text-mode`, config files, commit messages -- everywhere.

**It generates all candidate regions upfront.** On the first call to
`expreg-expand`, every expander function runs and produces a list of candidate
regions. These are filtered, sorted by size, and deduplicated. Subsequent calls
just pop the next region off the stack. This makes the behavior predictable and
easy to debug -- no more wondering why expand-region jumped to something
unexpected.

**It's tiny.** The entire package is a single file of a few hundred lines. Compare
that to expand-region's dozens of mode-specific files. Less code means fewer
bugs and easier maintenance.

## Customization

The expander functions are controlled by `expreg-functions`:

``` emacs-lisp
(expreg--subword    ; CamelCase subwords (when subword-mode is active)
 expreg--word       ; words and symbols
 expreg--list       ; parenthesized expressions
 expreg--string     ; string literals
 expreg--treesit    ; tree-sitter nodes
 expreg--comment    ; comments
 expreg--paragraph-defun) ; paragraphs and function definitions
```

There's also `expreg--sentence` available but not enabled by default -- useful
for prose:

``` emacs-lisp
(add-hook 'text-mode-hook
          (lambda ()
            (add-to-list 'expreg-functions #'expreg--sentence)))
```

## Should You Switch?

If you're using tree-sitter based major modes (and in 2026, you probably should
be), `expreg` gives you better, language-aware expansion for free. If you're still
on classic major modes, it's at least as good as `expand-region` thanks to the
syntax-table fallbacks.

The only reason to stick with expand-region is if you rely on some very specific
mode-specific expansion behavior that expand-region's hand-written functions
handle and expreg's generic approach doesn't. In practice, I haven't hit such a
case.

I've been using expreg for a while now across Clojure, OCaml, Emacs Lisp, and
various non-programming buffers. It just works. It's one of those small
packages that quietly improves your daily editing without demanding attention.
And like tree-sitter powered [code completion]({% post_url
2025-06-03-tree-sitter-powered-code-completion %}), it's another example of how
tree-sitter is reshaping what Emacs packages can do with minimal effort.

That's all I have for you today. Keep hacking!

[^1]: Naming is hard.
