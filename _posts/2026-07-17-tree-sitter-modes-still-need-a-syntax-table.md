---
layout: post
title: "Tree-sitter Modes Still Need a Syntax Table"
date: 2026-07-17 12:15 +0300
tags:
- Tree-sitter
- Major Modes
---

When you write a tree-sitter major mode, it's tempting to think the
parser handles everything. It doesn't. Tree-sitter drives font-lock,
indentation, and structural navigation, but a whole layer of everyday
Emacs behavior still runs on the humble **syntax table**: `forward-sexp`
and friends, `electric-pair-mode`, `delete-pair`, `forward-word`, and
anything built on `syntax-ppss`. (I wrote about that structural layer in
[Essential Structured Navigation and Editing Commands]({% post_url 2026-06-20-essential-structured-navigation-and-editing-commands %}).)

I got a sharp reminder of this while building
[neocaml](https://github.com/bbatsov/neocaml), my tree-sitter mode for
OCaml. The grammar parsed everything beautifully, yet `delete-pair` and
sexp motion kept misbehaving around a couple of OCaml constructs. The
fix turned out to be a classic tool that long predates tree-sitter:
`syntax-propertize-function`. Here's the story - it tripped me up for a
good while, so maybe it'll spare you some head-scratching.

<!--more-->

## The problem: characters a static syntax table can't classify

A syntax table assigns each *character* a single class: this one opens a
string, that one is a comment starter, this is a word constituent. That
works until a character means different things in different places. OCaml
has two such troublemakers.

**Character literals.** `'a'` is a character, and the single quotes
delimit it. But the same `'` also starts a *type variable* (`'a` in
`'a list`), and can appear inside identifiers as a prime (`x'`). So you
can't just declare `'` a string delimiter. And if you leave it as a
symbol constituent (the usual choice), a character literal whose contents
happen to be a delimiter wreaks havoc:

``` ocaml
let x = '"'   (* the " opens a string, as far as the syntax table knows *)
let y = '('   (* the ( is an unbalanced open paren *)
```

**Quoted string literals.** OCaml's `{|...|}` (and tagged `{foo|...|foo}`)
raw strings can contain anything - including `"`, `(*`, and friends -
which the syntax table reads as real string/comment delimiters:

``` ocaml
let z = {|a "b" (* not a comment *)|}
```

You can *see* the damage with `syntax-ppss`. With nothing but a static
table, point at the end of `let x = '"'` reports that you're inside a
string:

``` emacs-lisp
(nth 3 (syntax-ppss))  ;; => 34   (i.e. inside a string opened by ")
```

And every command that consults the syntax table inherits the confusion:
`C-M-f` walks off into nonsense, `delete-pair` grabs the wrong delimiter,
`electric-pair-mode` autopairs incorrectly.

Note that *font-lock* looks fine here - tree-sitter fontifies these
constructs correctly from the parse tree. That's exactly what makes the
bug sneaky: the buffer looks right, but the syntactic layer underneath is
lying.

## The fix: context-sensitive syntax with `syntax-propertize`

The escape hatch is
[`syntax-propertize-function`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Properties.html):
a buffer-local function that runs lazily over regions of the buffer and
applies `syntax-table` *text properties* to override the static table
where context demands it. Because the properties are attached to specific
positions, `'` can be a string delimiter in `'a'` and an ordinary symbol
character in `'a list` - in the same buffer.

The usual way to write one is with `syntax-propertize-rules`, which maps
regexps to the syntax classes to apply to their capture groups. Here's
the heart of neocaml's:

``` emacs-lisp
(defun neocaml--syntax-propertize (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ;; Character literals: 'a', '\n', '"', '(', ...
    ;; Mark both quotes as string delimiters so the contents are inert.
    ;; A closing quote is required, so type variables ('a) are untouched.
    ("\\_<\\('\\)\\(?:[^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
     (1 "\"") (2 "\""))
    ;; Quoted strings {tag|...|tag}: fence both ends so the body is inert.
    ("\\({\\)\\([[:lower:]_]*\\)|"
     (1 (let* ((tag (match-string 2))
               (close (save-excursion
                        (when (search-forward (concat "|" tag "}") end t)
                          (point)))))
          (when close
            (put-text-property (1- close) close
                               'syntax-table (string-to-syntax "|")))
          (string-to-syntax "|")))))
   (point) end))
```

and you install it in the mode body:

``` emacs-lisp
(setq-local syntax-propertize-function #'neocaml--syntax-propertize)
```

Two kinds of rules are at work:

- The *character literal* rule matches a complete `'...'` and gives both
  quotes string-quote syntax (`"`). The contents then sit *inside a
  string* as far as the scanner is concerned, so an inner `"` or `(` is
  inert. Crucially, the regexp requires a *closing* quote, so a bare
  `'a` type variable never matches and keeps its symbol syntax.
- The *quoted string* rule matches the opening `{tag|`, searches for the
  matching `|tag}`, and marks both the opening and closing delimiters with
  *generic string fence* syntax (`|`). Everything between two fences is a
  string, so embedded quotes and comment starters are neutralized.

With that in place, `syntax-ppss` tells the truth again, and `C-M-f`,
`delete-pair`, and `electric-pair-mode` all behave - including for the
`delete-pair` corner case I [wrote about earlier]({% post_url 2026-03-14-removing-paired-delimiters-in-emacs %}).

## Lessons for mode writers

A few things worth keeping in mind, whether or not you touch OCaml:

- Tree-sitter and the syntax table are different layers. The parser
  handles font-lock and structural queries, while the syntax table handles
  character-level motion and pairing. A great grammar doesn't save you
  from getting the syntax table right. (See also
  [Customizing Font-Lock in the Age of Tree-sitter]({% post_url 2026-03-08-customizing-font-lock-in-the-age-of-tree-sitter %}).)
- Reach for `syntax-propertize` whenever a character's role depends on
  context - raw strings, here-docs, regex literals, character literals,
  JSX, anything a single syntax class can't capture. It's exactly what
  `rust-ts-mode` and `c-ts-mode` reach for, too.
- The syntax-table text properties don't affect tree-sitter font-lock.
  They live on a separate channel, so you can fix the syntactic layer
  without disturbing your carefully tuned highlighting.
- Don't call `syntax-ppss` from inside your `syntax-propertize-function`.
  It's re-entrant (`syntax-propertize` is itself driven by `syntax-ppss`)
  and a fragile source of subtle bugs. Match constructs whole from their
  opening delimiter instead, as the quoted-string rule above does, rather
  than asking "am I currently inside a string?".
- Know your string classes. Class `"` (string quote) is delimited by the
  *same* character, while class `|` (generic string fence) pairs up
  independently of the specific character. The latter is handy for
  multi-character or asymmetric delimiters like `{tag|` ... `|tag}`.

None of this is new - `syntax-propertize` has been the right answer since
Emacs 24. But it's easy to forget it exists when you're deep in
tree-sitter land, and the symptoms (movement and pairing going subtly
wrong while the colors look perfect) are puzzling until you remember which
layer owns what.

Have you run into this while writing your own tree-sitter modes? I'd love
to hear how you tamed the syntax table in the comments.

That's all I have for you today. Keep those parens balanced!
