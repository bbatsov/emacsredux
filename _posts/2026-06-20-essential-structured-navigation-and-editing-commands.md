---
layout: post
title: "Essential Structured Navigation and Editing Commands"
date: 2026-06-20 10:00 +0300
tags:
- Editing
- Tree-sitter
- Navigation
---

Most of us learn Emacs one motion at a time -- `C-f` for a character, `M-f`
for a word, `C-n` for a line. Useful, but those commands don't know anything
about the *structure* of your code. Emacs has a whole other family of commands
that operate on balanced expressions and definitions instead, and once they
become muscle memory they're hard to give up.

Lisp hackers know these commands intimately -- they're the foundation
[paredit](https://paredit.org/) builds on. What's less appreciated is that they
work in plenty of other languages too. I've been leaning on them heavily while
building [neocaml](https://github.com/bbatsov/neocaml), my tree-sitter package
for OCaml programming, so I'll use OCaml for the examples here. The commands themselves are
general, though, and most of what follows applies to any structure-aware major
mode.

<!--more-->

## Sexps and defuns

Two terms show up everywhere in this corner of Emacs, and both are inherited
from its Lisp roots:

- A **sexp** ("symbolic expression") is a
  [balanced expression](https://www.gnu.org/software/emacs/manual/html_node/emacs/Expressions.html):
  a literal, an
  identifier, a parenthesized group, a list, a function application -- whatever
  the major mode considers one self-contained unit.
- A **defun** is a top-level definition. In Lisp that's a `defun`; in OCaml it's
  a `let` binding, a type definition, a module, and so on.

The names are Lispy, but the *concepts* are general, and that's the whole point.
Every command below is built on one of these two notions.

## Moving around

These are the everyday workhorses. In the examples, `█` marks point (the
cursor).

| Keybinding | Command | What it does |
|---|---|---|
| `C-M-f` | `forward-sexp` | Move forward over a balanced expression |
| `C-M-b` | `backward-sexp` | Move backward over a balanced expression |
| `C-M-d` | `down-list` | Move *into* a bracketed group |
| `C-M-u` | `backward-up-list` | Move *out* of the enclosing group |
| `C-M-a` | `beginning-of-defun` | Jump to the start of the current definition |
| `C-M-e` | `end-of-defun` | Jump to the end of the current definition |

The interesting thing about `forward-sexp` is that it moves over the *largest*
expression starting at point. At the head of a function application, that's the
whole call:

``` ocaml
let r = █List.map (fun x -> x + 1) numbers
```

After `C-M-f`:

``` ocaml
let r = List.map (fun x -> x + 1) numbers█
```

But inside a collection it steps over one element at a time, which is exactly
what you want when you're editing the elements:

``` ocaml
let xs = [ █aa; bb; cc ]
```

``` ocaml
let xs = [ aa█; bb; cc ]
```

`down-list` and `backward-up-list` are a great pair for repositioning. The
latter is especially nice in a structure-aware mode because it understands
*keyword-delimited* blocks, not just parens. From deep inside a `struct` body,
`C-M-u` climbs out to the enclosing module:

``` ocaml
module M = struct
  let x = 1
  let y = █2
end
```

``` ocaml
module M = █struct
  let x = 1
  let y = 2
end
```

## Editing with structure

Movement is only half the story. The same structural knowledge powers a set of
editing commands that are far more precise than their line- or character-based
cousins.

| Keybinding | Command | What it does |
|---|---|---|
| `C-M-SPC` | `mark-sexp` | Mark the next balanced expression |
| `C-M-k` | `kill-sexp` | Kill the expression after point |
| `C-M-t` | `transpose-sexps` | Swap the two expressions around point |
| `M-x delete-pair` | `delete-pair` | Remove a matched delimiter pair |
| `M-x raise-sexp` | `raise-sexp` | Replace the enclosing expression with the one at point |

`kill-sexp` removes a whole expression without you having to hunt for where it
ends:

``` ocaml
let xs = █(List.rev ys) @@ tl
```

``` ocaml
let xs = █ @@ tl
```

`transpose-sexps` swaps the expressions on either side of point -- a one-keystroke
way to fix arguments you passed in the wrong order:

``` ocaml
let path = Filename.concat dir █file
```

``` ocaml
let path = Filename.concat file dir█
```

(There's much more to say about transposition -- I covered the whole family in
[Transpose All The Things]({% post_url 2026-03-04-transpose-all-the-things %}).)

`delete-pair` unwraps a bracketed group: put point on the opening delimiter and
it deletes both it and its match.

``` ocaml
let area = pi *. █(r *. r)
```

``` ocaml
let area = pi *. █r *. r
```

`delete-pair` has a subtle dependency on structure that I ran into head-on while
working on neocaml -- it relies on the mode's notion of a sexp to find the
matching delimiter. I wrote that adventure up separately in
[Removing Paired Delimiters in Emacs]({% post_url 2026-03-14-removing-paired-delimiters-in-emacs %}).

Finally, `raise-sexp` replaces the *enclosing* expression with the
sub-expression at point. It's the fastest way I know to strip a wrapper -- here,
dropping a `Some` around a call:

``` ocaml
let result = process (Some █(find_opt key tbl))
```

``` ocaml
let result = process █(find_opt key tbl)
```

One caveat: `raise-sexp` needs something to raise *into*. Invoke it at the top
level of a binding, where nothing surrounds the expression, and it'll complain
about unbalanced parentheses. Reach for it inside a call, a list, or a
parenthesized expression.

## The same key, different behavior

Here's the part that trips people up: **these commands behave differently from
one major mode to the next**, because each mode gets to define what "an
expression" or "a definition" actually means. They do it through a few
buffer-local hooks -- chiefly `forward-sexp-function` (which backs `C-M-f`/`C-M-b`
and everything built on them, including `kill-sexp`, `transpose-sexps`, and
`delete-pair`) and `beginning-of-defun-function` / `end-of-defun-function` for
defun motion. What plugs into those hooks has changed a lot over the years.

### The classic backing: syntax tables and regexps

For most of Emacs's history, structural commands rested on the
[syntax table](https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html) --
the per-mode table that classifies each character as an open or close delimiter,
a string quote, a word or symbol constituent, and so on.
[`scan-sexps` and `scan-lists`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Motion-via-Parsing.html)
walk the buffer counting delimiters according to that table, and
that's what `forward-sexp` falls back on when a mode sets nothing special. It's
wonderfully cheap and works in *any* buffer, even plain text -- but it's purely
lexical. It counts brackets and respects quoting, yet has no idea what a
function or a block actually *is*. That's why vanilla `backward-up-list` can't
climb out of a `begin ... end` or OCaml `struct ... end` block: those aren't
bracket characters, so as far as the syntax table is concerned they don't exist.

Modes that wanted more had two classic options:

- [SMIE](https://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html)
  (the Simple Minded Indentation Engine). Modes like `ruby-mode` and
  `octave-mode` feed SMIE a small operator-precedence grammar, and `smie-setup`
  points `forward-sexp-function` at `smie-forward-sexp-command`. Suddenly `C-M-f`
  understands keyword-delimited blocks like `if ... end`, not just parens.
- Regexps for definitions. A `beginning-of-defun-function` that scans for a
  header pattern, and an `imenu-generic-expression` for the imenu index. Quick to
  write, but heuristic -- easily fooled by code inside strings and comments, or by
  unconventional formatting.

### The tree-sitter backing: a real parse tree

Tree-sitter modes answer the same questions from an actual
[concrete syntax tree](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html).
`forward-sexp-function` becomes `treesit-forward-sexp`; on Emacs 30+, a mode
declares named "things" -- `sexp`, `list`, `sentence`, `defun`, `text` -- via
`treesit-thing-settings`, and the navigation commands consult them. Defun motion
goes through `treesit-beginning-of-defun` (driven by `treesit-defun-type-regexp`),
and imenu through `treesit-simple-imenu-settings`.

Because it's the real grammar rather than a delimiter count, it's accurate
exactly where the lexical approach has to guess. That's why, earlier, `C-M-f`
could treat the whole `List.map ...` application as one node, and `C-M-u` could
climb out of a paren-less `struct` block -- and why a stray brace inside a string
never throws it off. The price of admission is a compiled grammar and a
reasonably recent Emacs.[^1]

This layering is also why a command like `delete-pair` can quietly misbehave: it
leans on the mode's `forward-sexp-function` to find the matching delimiter, so if
that disagrees with the buffer's syntax table you get surprising deletions.
Getting these foundations right is a big part of what makes a structure-aware
major mode feel solid.

## What about paredit?

As mentioned in the beginning of this articles, all of this probably reminds
(some of) you of [paredit](https://paredit.org/) -- and it should. paredit takes
the same structural ideas and turns them up to eleven, adding slurping, barfing,
splicing, and the famous guarantee that your parens always stay balanced. The
built-in commands here are the humbler, mode-agnostic ancestors of those ideas.

For non-Lisp languages there are spiritual successors built on tree-sitter.
[Combobulate](https://github.com/mickeynp/combobulate) is the most ambitious --
a full structural editing and navigation package for tree-sitter modes, with
drag, splice, and "move by sibling" commands. [puni](https://github.com/AmaiKinono/puni)
takes a different tack, offering paredit-style soft deletion that works across
many languages by leaning on `forward-sexp-function`. (If you do mix paredit
with other tools, mind the keybindings -- I wrote about some common clashes in
[Paredit Keybinding Conflicts]({% post_url 2026-03-27-paredit-keybinding-conflicts %}).)

## More than movement and editing

Once a mode knows how to find sexps and defuns, a surprising number of other
features come along for the ride -- all powered by the same structural framework:

- `narrow-to-defun` (`C-x n d`) -- narrow the buffer to just the current
  definition. Wonderful for focusing on one function at a time, and it uses the
  exact same defun detection as `C-M-a`/`C-M-e`.
- `which-function-mode` -- show the name of the definition point is in, in
  the mode line. It needs the mode to know where definitions begin and end.
- [`imenu`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Imenu.html) --
  jump to a definition by name. Same structural underpinning.
- Folding -- `outline-minor-mode`, `hs-minor-mode`, and the tree-sitter
  outline support on Emacs 30+ all fold along structural boundaries.
- Expanding the region -- repeatedly hitting `C-M-SPC` grows the selection
  by sexp, and packages like [expreg]({% post_url 2026-03-03-expreg-expand-region-reborn %})
  generalize that to grow by structural node.

## Wrapping up

The `C-M-`-prefixed commands are some of the best returns on investment in all
of Emacs. They're not Lisp-only, they're not tree-sitter-only, and they quietly
get smarter as major modes teach Emacs about their syntax. Learn the handful in
the tables above -- `C-M-f`/`C-M-b`, `C-M-u`/`C-M-d`, `C-M-SPC`, `C-M-k`,
`C-M-a`/`C-M-e` -- and you'll feel the difference in every language you touch.

And if a couple of these (`delete-pair`, `raise-sexp`) feel essential but lack
default bindings, give them keys in your major mode of choice. Your future self,
mid-refactor, will thank you.

That's all I have for you today. Keep hacking!

[^1]: Tree-sitter support was introduced in Emacs 29, but it got significantly better in Emacs 30.
