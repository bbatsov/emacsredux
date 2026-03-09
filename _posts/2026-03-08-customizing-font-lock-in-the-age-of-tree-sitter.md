---
layout: post
title: Customizing Font-Lock in the Age of Tree-sitter
date: 2026-03-08 10:30 +0200
tags:
- Tree-sitter
- Configuration
---

I recently wrote about
[building major modes with Tree-sitter](https://batsov.com/articles/2026/02/27/building-emacs-major-modes-with-treesitter-lessons-learned/)
over on [batsov.com](https://batsov.com), covering the mode author's perspective. But what about the
*user's* perspective? If you're using a Tree-sitter-powered major mode, how do
you actually customize the highlighting?

This is another article in a recent streak inspired by my work on
[neocaml](https://github.com/bbatsov/neocaml),
[clojure-ts-mode](https://github.com/clojure-emacs/clojure-ts-mode), and
[asciidoc-mode](https://github.com/bbatsov/asciidoc-mode). Building three
Tree-sitter modes across very different languages has given me a good feel for
both sides of the font-lock equation -- and I keep running into users who are
puzzled by how different the new system is from the old regex-based world.

This post covers what changed, what you can control, and how to make Tree-sitter
font-lock work exactly the way you want.

## The Old World: Regex Font-Lock

Traditional font-lock in Emacs actually has two phases. First, *syntactic
fontification* handles comments and strings using the buffer's syntax table and
`parse-partial-sexp` (implemented in C) -- this isn't regexp-based at all.
Second, *keyword fontification* runs the regexps in `font-lock-keywords` against
the buffer text to highlight everything else: language keywords, types, function
names, and so on. When people talk about "regex font-lock," they usually mean
this second phase, which is where most of the mode-specific highlighting lives
and where most of the customization happens.

If you wanted to customize it, you'd manipulate `font-lock-keywords` directly:

```emacs-lisp
;; Add a custom highlighting rule in the old world
(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(FIXME\\|TODO\\)\\>" 1 'font-lock-warning-face prepend)))
```

The downsides are well-known: regexps can't understand nesting, they break on
multi-line constructs, and getting them right for a real programming language is
a never-ending battle of edge cases.

## The New World: Tree-sitter Font-Lock

Tree-sitter font-lock is fundamentally different. Instead of matching text with
regexps, it queries the *syntax tree*. A major mode defines
`treesit-font-lock-settings` -- a list of Tree-sitter queries paired with faces.
Each query pattern matches node types in the parse tree, not text patterns.

This means highlighting is structurally correct by definition. A string is
highlighted as a string because the parser identified it as a string node, not
because a regexp happened to match quote characters. If the code has a syntax
error, the parser still produces a (partial) tree, and highlighting degrades
gracefully instead of going haywire.

There's also a significant performance difference. With regex font-lock, every
regexp in `font-lock-keywords` runs against every line in the visible region on
each update -- more rules means linearly more work, and a complex major mode can
easily have dozens of regexps. Poorly written patterns with nested quantifiers
can trigger catastrophic backtracking, causing visible hangs on certain inputs.
Multi-line font-lock (via `font-lock-multiline` or `jit-lock-contextually`)
makes things worse, requiring re-scanning of larger regions that's both expensive
and fragile. Tree-sitter sidesteps all of this: after the initial parse, edits
only re-parse the changed portion of the syntax tree, and font-lock queries run
against the already-built tree rather than scanning raw text. The result is
highlighting that scales much better with buffer size and rule complexity.

The trade-off is that customization works differently. You can't just add a
regexp to a list anymore. But the new system offers its own kind of flexibility,
and in many ways it's more powerful.

**Note:** The Emacs manual covers Tree-sitter font-lock in the
[Parser-based Font Lock](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Font-Lock.html)
section. For the full picture of Tree-sitter integration in Emacs, see
[Parsing Program Source](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html).

## Feature Levels: The Coarse Knob

Every Tree-sitter major mode organizes its font-lock rules into *features* --
named groups of related highlighting rules. Features are then arranged into 4
levels, from minimal to maximal. The Emacs manual recommends the following
conventions for what goes into each level:

- **Level 1:** The absolute minimum -- typically `comment` and `definition`
- **Level 2:** Key language constructs -- `keyword`, `string`, `type`
- **Level 3:** Everything that can be reasonably fontified (this is the default level)
- **Level 4:** Marginally useful highlighting -- things like `bracket`, `delimiter`, `operator`

In practice, many modes don't follow these conventions precisely. Some put
`number` at level 2, others at level 3. Some include `variable` at level 1,
others at level 4. The inconsistency across modes means that setting
`treesit-font-lock-level` to the same number in different modes can give you
quite different results -- which is one more reason you might want the
fine-grained control described in the next section.[^1]

It's also worth noting that the feature names themselves are not standardized.
There are many common ones you'll see across modes -- `comment`, `string`,
`keyword`, `type`, `number`, `bracket`, `operator`, `definition`, `function`,
`variable`, `constant`, `builtin` -- but individual modes often define features
specific to their language. Clojure has `quote`, `deref`, and `tagged-literals`;
OCaml might have `attribute`; a markup language mode might have `heading` or
`link`. Different modes also vary in how granular they get: some expose a rich
set of features that let you fine-tune almost every aspect of highlighting, while
others are more spartan and stick to the basics.

The bottom line is that you'll always have to check what your particular mode
offers. The easiest way is `M-x describe-variable RET
treesit-font-lock-feature-list` in a buffer using that mode -- it shows all
features organized by level. You can also inspect the mode's source directly by
looking at how it populates `treesit-font-lock-settings` (try `M-x find-library`
to jump to the mode's source).

For example, clojure-ts-mode defines:

| Level | Features                                                    |
|-------|-------------------------------------------------------------|
| 1     | `comment`, `definition`, `variable`                         |
| 2     | `keyword`, `string`, `char`, `symbol`, `builtin`, `type`    |
| 3     | `constant`, `number`, `quote`, `metadata`, `doc`, `regex`   |
| 4     | `bracket`, `deref`, `function`, `tagged-literals`           |

And neocaml:

| Level | Features                                                    |
|-------|-------------------------------------------------------------|
| 1     | `comment`, `definition`                                     |
| 2     | `keyword`, `string`, `number`                               |
| 3     | `attribute`, `builtin`, `constant`, `type`                  |
| 4     | `operator`, `bracket`, `delimiter`, `variable`, `function`  |

The default level is 3, which is a reasonable middle ground for most people. You
can change it globally:

```emacs-lisp
(setq treesit-font-lock-level 4)  ;; maximum highlighting
```

Or per-mode via a hook:

```emacs-lisp
(defun my-clojure-ts-font-lock ()
  (setq-local treesit-font-lock-level 2))  ;; minimal: just keywords and strings

(add-hook 'clojure-ts-mode-hook #'my-clojure-ts-font-lock)
```

This is the equivalent of the old `font-lock-maximum-decoration` variable, but
more principled -- features at each level are explicitly chosen by the mode
author rather than being an arbitrary "how much highlighting do you want?" dial.

**Note:** The Emacs manual describes this system in detail under
[Font Lock and Syntax](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Font-Lock.html#Font-Lock-and-Syntax).

## Cherry-Picking Features: The Fine Knob

Levels are a blunt instrument. What if you want operators and variables (level 4)
but not brackets and delimiters (also level 4)? You can't express that with a
single number.

Enter `treesit-font-lock-recompute-features`. This function lets you explicitly
enable or disable individual features, regardless of level:

```emacs-lisp
(defun my-neocaml-font-lock ()
  (treesit-font-lock-recompute-features
   '(comment definition keyword string number
     attribute builtin constant type operator variable)  ;; enable
   '(bracket delimiter function)))                       ;; disable

(add-hook 'neocaml-base-mode-hook #'my-neocaml-font-lock)
```

You can also call it interactively with `M-x treesit-font-lock-recompute-features`
to experiment in the current buffer before committing to a configuration.

This is something that was hard to do cleanly in the old regex world. You'd have
to dig into `font-lock-keywords`, figure out which entries corresponded to which
syntactic elements, and surgically remove them. With Tree-sitter, it's a
declarative list of names.

## Customizing Faces

This part works the same as before -- faces are faces. Tree-sitter modes use
the standard `font-lock-*-face` family, so your theme applies automatically.
If you want to tweak a specific face:

```emacs-lisp
(custom-set-faces
 '(font-lock-type-face ((t (:foreground "DarkSeaGreen4"))))
 '(font-lock-property-use-face ((t (:foreground "DarkOrange3")))))
```

One thing to note: Tree-sitter modes use some of the newer faces introduced in
Emacs 29, like `font-lock-operator-face`, `font-lock-bracket-face`,
`font-lock-number-face`, `font-lock-property-use-face`, and
`font-lock-escape-face`. These didn't exist in the old world (there was no
concept of "operator highlighting" in traditional font-lock), so older themes
may not define them. If your theme makes operators and variables look the same,
that's why -- the theme predates these faces.

## Adding Custom Rules

This is where Tree-sitter font-lock really shines compared to the old system.
Instead of writing regexps, you write Tree-sitter queries that match on the
actual syntax tree.

Say you want to distinguish block-delimiting keywords (`begin`/`end`,
`struct`/`sig`) from control-flow keywords (`if`/`then`/`else`) in OCaml:

```emacs-lisp
(defface my-block-keyword-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for block-delimiting keywords.")

(defun my-neocaml-block-keywords ()
  (setq treesit-font-lock-settings
        (append treesit-font-lock-settings
                (treesit-font-lock-rules
                 :language (treesit-parser-language
                            (car (treesit-parser-list)))
                 :override t
                 :feature 'keyword
                 '(["begin" "end" "struct" "sig" "object"]
                   @my-block-keyword-face))))
  (treesit-font-lock-recompute-features))

(add-hook 'neocaml-base-mode-hook #'my-neocaml-block-keywords)
```

The `:override t` is important -- without it, the new rule won't overwrite
faces already applied by the mode's built-in rules. And the `:feature` keyword
assigns the rule to a feature group, so it respects the level/feature system.

**Note:** The full query syntax is documented in the
[Pattern Matching](https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-Matching.html)
section of the Emacs manual -- it covers node types, field names, predicates,
wildcards, and more.

For comparison, here's what you'd need in the old regex world to highlight a
specific set of keywords with a different face:

```emacs-lisp
;; Old world: fragile, doesn't understand syntax
(font-lock-add-keywords 'some-mode
  '(("\\<\\(begin\\|end\\|struct\\|sig\\)\\>" . 'my-block-keyword-face)))
```

The regex version looks simpler, but it'll match `begin` inside strings,
comments, and anywhere else the text appears. The Tree-sitter version only
matches actual keyword nodes in the syntax tree.

## Exploring the Syntax Tree

The killer feature for customization is `M-x treesit-explore-mode`. It opens a
live view of the syntax tree for the current buffer. As you move point, the
explorer highlights the corresponding node and shows its type, field name, and
position.

This is indispensable when writing custom font-lock rules. Want to know what
node type OCaml labels are? Put point on one, check the explorer: it's
`label_name`. Want to highlight it? Write a query for `(label_name)`. No more
guessing what regexp might work.

Another useful tool is `M-x treesit-inspect-node-at-point`, which shows
information about the node at point in the echo area without opening a separate
window.

## The Cheat Sheet

Here's a quick reference for the key differences:

| Aspect                  | Regex font-lock                      | Tree-sitter font-lock                     |
|-------------------------|--------------------------------------|-------------------------------------------|
| Rules defined by        | `font-lock-keywords`                 | `treesit-font-lock-settings`              |
| Matching mechanism      | Regular expressions on text          | Queries on syntax tree nodes              |
| Granularity control     | `font-lock-maximum-decoration`       | `treesit-font-lock-level` + features      |
| Adding rules            | `font-lock-add-keywords`             | Append to `treesit-font-lock-settings`    |
| Removing rules          | `font-lock-remove-keywords`          | `treesit-font-lock-recompute-features`    |
| Debugging               | `re-builder`                         | `treesit-explore-mode`                    |
| Handles nesting         | Poorly                               | Correctly (by definition)                 |
| Multi-line constructs   | Fragile                              | Works naturally                           |
| Performance             | O(n) per regexp per line             | Incremental, only re-parses changes       |

## Closing Thoughts

The shift from regex to Tree-sitter font-lock is one of the bigger
under-the-hood changes in modern Emacs. The customization model is different --
you're working with structured queries instead of text patterns -- but once you
internalize it, it's arguably more intuitive. You say "highlight this kind of
syntax node" instead of "highlight text that matches this pattern and hope it
doesn't match inside a string."

The feature system with its levels, cherry-picking, and custom rules gives you
more control than the old `font-lock-maximum-decoration` ever did. And
`treesit-explore-mode` makes it easy to discover what's available.

If you haven't looked at your Tree-sitter mode's font-lock features yet, try
`M-x describe-variable RET treesit-font-lock-feature-list` in a Tree-sitter
buffer. You might be surprised by how much you can tweak.

[^1]: Writing this article has been more helpful than I expected -- halfway through, I realized my own neocaml had `function` banished to level 4 and `number` promoted to level 2. Physician, heal thyself.
