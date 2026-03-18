---
layout: post
title: Tree-sitter Font-Lock and Indentation in Comint Buffers
date: 2026-03-17 16:30 +0200
tags:
- Tree-sitter
- REPL
---

If you maintain a tree-sitter major mode that has a REPL (comint) companion,
you've probably wondered: can the REPL input get the same syntax highlighting
and indentation as source buffers? The answer is yes -- and the infrastructure
has been in Emacs since 29.1. It's just not widely known yet.

<!--more-->

I ran into this while working on [neocaml](https://github.com/bbatsov/neocaml),
my tree-sitter major mode for OCaml. The OCaml REPL buffer used simple
regex-based `font-lock-keywords` for input, which was definitely a step backward from
the rich highlighting in source buffers.

Initially I didn't even bother to research using `tree-sitter` font-lock in
comint, as assumed that would be something quite complicated. Turns out the fix
was surprisingly easy.

## Font-Lock: `comint-fontify-input-mode`

Emacs 29.1 introduced `comint-fontify-input-mode`, a minor mode that fontifies
input regions in comint buffers through an **indirect buffer**. The idea is
simple:

1. You tell comint which major mode to use for input via
   `comint-indirect-setup-function`.
2. Comint creates an indirect buffer and runs that mode in it.
3. When fontifying, comint splits the buffer into output and input regions.
   Output gets the comint buffer's own font-lock; input is fontified in the
   indirect buffer using the full major mode -- including tree-sitter.

Here's all it took for neocaml's REPL:

```emacs-lisp
(define-derived-mode neocaml-repl-mode comint-mode "OCaml-REPL"
  ;; ... existing setup ...

  ;; Tree-sitter fontification for REPL input
  (setq-local comint-indirect-setup-function #'neocaml-mode)
  (comint-fontify-input-mode))
```

That's it. REPL input now gets the exact same tree-sitter font-lock as `.ml`
buffers. The existing `font-lock-keywords` for output (error messages, warnings,
`val`/`type` results) keep working as before -- they only apply to output
regions.

**Important:** `comint-fontify-input-mode` is incompatible with
`comint-use-prompt-regexp` -- the two features can't be active at the same
time. Most modern comint-derived modes don't set `comint-use-prompt-regexp` to
`t`, so this usually isn't an issue.

### Who's Already Doing This

Two built-in modes use this approach:

- **shell.el** sets up `sh-mode` in the indirect buffer (enabled by default via
  `shell-fontify-input-enable`)
- **ielm.el** sets up `emacs-lisp-mode` (enabled by default via
  `ielm-fontify-input-enable`)

On the third-party side, [inf-lua](https://github.com/nverno/inf-lua) and
[ts-repl](https://github.com/nverno/ts-repl) both use this pattern with
tree-sitter modes. And now there's also `neocaml`, of course. :-)

## Indentation: `comint-indent-input-line-default`

Comint also provides indentation delegation via `comint-indent-input-line-default`
and `comint-indent-input-region-default`. When you set these as
`indent-line-function` and `indent-region-function`, pressing TAB on an input
line delegates indentation to the indirect buffer's `indent-line-function` --
which will be `treesit-indent` if the indirect buffer runs a tree-sitter mode.

```emacs-lisp
(setq-local indent-line-function #'comint-indent-input-line-default)
(setq-local indent-region-function #'comint-indent-input-region-default)
```

**shell.el** already does this. For your own REPL modes, you can add these two
lines alongside the font-lock setup.

### The Caveat

Here's where things get tricky. Tree-sitter parsers are **shared between
indirect and base buffers** (this is by design -- see
[bug#59693](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59693)). When
`treesit-indent` runs in the indirect buffer, the parser it uses sees the
**entire** comint buffer -- prompts, output, previous commands, everything. The
parse tree will be full of errors from non-code content.

Font-lock handles this gracefully because `comint-fontify-input-mode` only
applies fontification results to input regions, so garbled parses of output
regions are harmless. But indentation is different -- `treesit-indent` looks at
the node context around point, and a broken parse tree can confuse it.

In practice, it works better than you'd expect.[^1] For simple multi-line
expressions, the local tree-sitter nodes at the cursor position are often
correct enough for reasonable indentation. But for deeply nested multi-line
input, the results can be off.

Because of this, I chose not to enable indentation delegation by default in
neocaml's REPL. Instead, it's documented as an opt-in configuration for
adventurous users.

## The Recipe

If you maintain a tree-sitter mode with a comint REPL, here's the minimal
pattern:

```emacs-lisp
(define-derived-mode my-repl-mode comint-mode "My-REPL"
  ;; ... your existing setup ...

  ;; Font-lock: full tree-sitter highlighting for input
  (setq-local comint-indirect-setup-function #'my-ts-mode)
  (comint-fontify-input-mode)

  ;; Indentation: delegate to tree-sitter (experimental)
  (setq-local indent-line-function #'comint-indent-input-line-default)
  (setq-local indent-region-function #'comint-indent-input-region-default))
```

Consider making these features opt-in via defcustoms, especially the indentation
part. And remember that your existing `font-lock-keywords` for output
highlighting (errors, warnings, result values) will continue working -- they
don't conflict with `comint-fontify-input-mode`.

## The End

That's it. Two overlooked comint features, a few lines of setup, and your REPL
goes from basic regex highlighting to full tree-sitter support.

Keep hacking!

[^1]: Depends on your expectations, of course.
