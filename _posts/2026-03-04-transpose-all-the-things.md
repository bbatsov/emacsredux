---
layout: post
title: Transpose All The Things
date: 2026-03-04 10:00 +0200
tags:
- Editing
- Tree-sitter
---

Most Emacs users know `C-t` to swap two characters and `M-t` to swap two words.
Some know `C-x C-t` for swapping lines. But the transpose family goes deeper
than that, and with tree-sitter in the picture, things get really interesting.

Let's take a tour.

## The Classics

The three transpose commands everyone knows (or should know):

| Keybinding | Command | What it does |
|---|---|---|
| `C-t` | `transpose-chars` | Swap the character before point with the one after |
| `M-t` | `transpose-words` | Swap the word before point with the one after |
| `C-x C-t` | `transpose-lines` | Swap the current line with the one above |

These are purely textual -- they don't care about syntax, language, or
structure. They work the same in an OCaml buffer, an email draft, or a
shell script. Simple and reliable.

One thing worth noting: `transpose-lines` is often used not for literal
transposition but as a building block for [moving lines up and
down]({% post_url 2013-04-02-move-current-line-up-or-down %}).

## The Overlooked Ones

Here's where it gets more interesting. Emacs has two more transpose
commands that most people never discover:

**`transpose-sentences`** (no default keybinding)

This swaps two sentences around point. In text modes, a "sentence" is
determined by `sentence-end` (typically a period followed by whitespace). In
programming modes... well, it depends. More on this below.

**`transpose-paragraphs`** (no default keybinding)

Swaps two paragraphs. A paragraph is separated by blank lines by default. Less
useful in code, but handy when editing prose or documentation.

Neither command has a default keybinding, which probably explains why they're so
obscure. If you write a lot of prose in Emacs, binding `transpose-sentences` to
something convenient is worth considering.

## The MVP: transpose-sexps

`C-M-t` (`transpose-sexps`) is the most powerful of the bunch. It
swaps two "balanced expressions" around point. What counts as a balanced
expression depends on the mode:

**In Lisp modes**, a sexp is what you'd expect -- an atom, a string, or a
parenthesized form:

```emacs-lisp
;; Before: point after `bar`
(foo bar| baz)
;; C-M-t →
(foo baz bar)
```

**In other programming modes**, "sexp" maps to whatever the mode considers a
balanced expression -- identifiers, string literals, parenthesized groups,
function arguments:

```python
# Before: point after `arg1`
def foo(arg1|, arg2):
# C-M-t →
def foo(arg2, arg1):
```

```ocaml
(* Before: point after `two` *)
foobar two| three
(* C-M-t → *)
foobar three two
```

This is incredibly useful for reordering function arguments, swapping `let`
bindings, or rearranging list elements. The catch is that "sexp" is a
Lisp-centric concept, and in non-Lisp languages the results can sometimes be
surprising -- the mode has to define what constitutes a balanced expression, and
that definition doesn't always match your intuition.

## How Tree-sitter Changes Things

Tree-sitter gives Emacs a full abstract syntax tree (AST) for every buffer, and
this fundamentally changes how structural commands work.

### Sexp Navigation and Transposition

On Emacs 30+, tree-sitter major modes can define a `sexp` "thing" in
`treesit-thing-settings`. This tells Emacs which AST nodes count as balanced
expressions. When this is configured, `transpose-sexps` (`C-M-t`) uses
`treesit-transpose-sexps` under the hood, walking the parse tree to find
siblings to swap instead of relying on syntax tables.

The result is more reliable transposition in languages where syntax-table-based
sexp detection struggles. OCaml's nested `match` arms, Python's indentation-based
blocks, Go's composite literals -- tree-sitter understands them all.

That said, the Emacs 30 implementation of `treesit-transpose-sexps` has some
rough edges (it sometimes picks the wrong level of the AST). Emacs 31 rewrites
the function to work more reliably.[^1]

### Sentence Navigation and Transposition

This is where things get quietly powerful. On Emacs 30+, tree-sitter modes can
also define a `sentence` thing in `treesit-thing-settings`. In a programming
context, "sentence" typically maps to top-level or block-level statements --
`let` bindings, type definitions, function definitions, imports, etc.

Once a mode defines this, `M-a` and `M-e` navigate between these constructs, and
`transpose-sentences` swaps them:

```ocaml
(* Before *)
let x = 42
let y = 17

(* M-x transpose-sentences → *)
let y = 17
let x = 42
```

```python
# Before
import os
import sys

# M-x transpose-sentences →
import sys
import os
```

This is essentially "transpose definitions" or "transpose statements" for free,
with no custom code needed beyond the `sentence` definition.

## Beyond the Built-ins

If the built-in transpose commands aren't enough, several packages extend the
concept:

**[combobulate](https://github.com/mickeynp/combobulate)** is the most
comprehensive tree-sitter structural editing package. Its `combobulate-drag-up`
(`M-P`) and `combobulate-drag-down` (`M-N`) commands swap the current AST node
with its previous or next sibling. This is like `transpose-sexps` but more
predictable -- it uses tree-sitter's sibling relationships directly, so it works
consistently for function parameters, list items, dictionary entries, HTML
attributes, and more.

For simpler needs, packages like
[drag-stuff](https://github.com/rejeep/drag-stuff.el) and
[move-text](https://github.com/emacsfodder/move-text) provide line and region
dragging without tree-sitter awareness. They're less precise but work everywhere.

## Wrapping Up

Here's the complete transpose family at a glance:

| Keybinding | Command | Tree-sitter aware? |
|---|---|---|
| `C-t` | `transpose-chars` | No |
| `M-t` | `transpose-words` | No |
| `C-x C-t` | `transpose-lines` | No |
| `C-M-t` | `transpose-sexps` | Yes (Emacs 30+) |
| (unbound) | `transpose-sentences` | Indirectly (Emacs 30+) |
| (unbound) | `transpose-paragraphs` | No |

The first three are textual workhorses that haven't changed much in decades.
`transpose-sexps` has been quietly upgraded by tree-sitter into something much
more capable. And `transpose-sentences` is the sleeper hit -- once your
tree-sitter mode defines what a "sentence" is in your language, you get
structural statement transposition for free.

That's all I have for you today. Keep hacking!

[^1]: See [bug#60655](https://debbugs.gnu.org/60655) for the gory details.
