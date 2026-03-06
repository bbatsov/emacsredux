---
layout: post
title: Mastering Compilation Mode
date: 2026-03-06 11:30 +0200
tags:
- Compilation
- Programming
---

I've been using Emacs for over 20 years. I've always used `M-x compile` and
`next-error` without thinking much about them -- you run a build, you jump to
errors, life is good. But recently, while working on
[neocaml](https://github.com/bbatsov/neocaml) (a Tree-sitter-based OCaml major
mode), I had to write a custom compilation error regexp and learned that
`compile.el` is far more sophisticated and extensible than I ever appreciated.

This post is a deep dive into compilation mode -- how it works, how to customize
it, and how to build on top of it.

## The Basics

If you're not already using `M-x compile`, start today. It runs a shell command,
captures the output in a `*compilation*` buffer, and parses error messages so you
can jump directly to the offending source locations.

The essential keybindings in a compilation buffer:

| Keybinding  | Command                        | What it does                                      |
|-------------|--------------------------------|---------------------------------------------------|
| `g`         | `recompile`                    | Re-run the last compilation command               |
| `M-n`       | `compilation-next-error`       | Move to the next error message                    |
| `M-p`       | `compilation-previous-error`   | Move to the previous error message                |
| `RET`       | `compile-goto-error`           | Jump to the source location of the error at point |
| `C-c C-f`   | `next-error-follow-minor-mode` | Auto-display source as you move through errors    |

But the real power move is using `next-error` and `previous-error` (`M-g n` and
`M-g p`) from *any* buffer. You don't need to be in the compilation buffer --
Emacs tracks the last buffer that produced errors and jumps you there. This works
across compile, grep, occur, and any other mode that produces error-like output.

**Pro tip:** `M-g M-n` and `M-g M-p` do the same thing as `M-g n` / `M-g p` but
are easier to type since you can hold Meta throughout.

## How Error Parsing Actually Works

Here's the part that surprised me. Compilation mode doesn't have a single regexp
that it tries to match against output. Instead, it has a *list* of regexp
entries, and it tries **all of them** against every line. The list lives in two
variables:

- `compilation-error-regexp-alist` -- a list of symbols naming active entries
- `compilation-error-regexp-alist-alist` -- an alist mapping those symbols to
  their actual regexp definitions

Emacs ships with dozens of entries out of the box -- for GCC, Java, Ruby,
Python, Perl, Gradle, Maven, and many more. You can see all of them with:

```emacs-lisp
(mapcar #'car compilation-error-regexp-alist-alist)
```

Each entry in the alist has this shape:

```
(SYMBOL REGEXP FILE LINE COLUMN TYPE HYPERLINK HIGHLIGHT...)
```

Where:

- **REGEXP** -- the regular expression to match
- **FILE** -- group number (or function) for the filename
- **LINE** -- group number (or cons of start/end groups) for the line
- **COLUMN** -- group number (or cons of start/end groups) for the column
- **TYPE** -- severity: 2 = error, 1 = warning, 0 = info (can also be a cons for conditional severity)
- **HYPERLINK** -- group number for the clickable portion
- **HIGHLIGHT** -- additional faces to apply

The TYPE field is particularly interesting. It can be a cons cell `(WARNING-GROUP
. INFO-GROUP)`, meaning "if group N matched, it's a warning; if group M matched,
it's info; otherwise it's an error." This is how a single regexp can handle
errors, warnings, and informational messages.

## A Real-World Example: OCaml Errors

Let me show you what I built for neocaml. OCaml compiler output looks like this:

```
File "foo.ml", line 10, characters 5-12:
10 |   let x = bad_value
              ^^^^^^^
Error: Unbound value bad_value
```

Warnings:

```
File "foo.ml", line 3, characters 6-7:
3 | let _ x = ()
          ^
Warning 27 [unused-var-strict]: unused variable x.
```

And ancillary locations (indented 7 spaces):

```
File "foo.ml", line 5, characters 0-20:
5 | let f (x : int) = x
    ^^^^^^^^^^^^^^^^^^^^
       File "foo.ml", line 10, characters 6-7:
10 |   f "hello"
          ^
Error: This expression has type string but ...
```

One regexp needs to handle all of this. Here's the (slightly simplified) entry:

```emacs-lisp
(push `(ocaml
        ,neocaml--compilation-error-regexp
        3                                    ; FILE = group 3
        (4 . 5)                              ; LINE = groups 4-5
        (6 . neocaml--compilation-end-column) ; COLUMN = group 6, end via function
        (8 . 9)                              ; TYPE = warning if group 8, info if group 9
        1                                    ; HYPERLINK = group 1
        (8 font-lock-function-name-face))    ; HIGHLIGHT group 8
      compilation-error-regexp-alist-alist)
```

A few things worth noting:

- The **COLUMN** end position uses a function instead of a group number.
  OCaml's end column is exclusive, but Emacs expects inclusive, so
  `neocaml--compilation-end-column` subtracts 1.
- The **TYPE** cons `(8 . 9)` means: if group 8 matched (Warning/Alert text),
  it's a warning; if group 9 matched (7-space indent), it's info; otherwise
  it's an error. Three severity levels from one regexp.
- The entry is registered globally in `compilation-error-regexp-alist-alist`
  because `*compilation*` buffers aren't in any language-specific mode. Every
  active entry is tried against every line.

## Adding Your Own Error Regexp

You don't need to be writing a major mode to add your own entry. Say you're
working with a custom linter that outputs:

```
[ERROR] src/app.js:42:10 - Unused import 'foo'
[WARN] src/app.js:15:3 - Missing return type
```

You can teach compilation mode about it:

```emacs-lisp
(with-eval-after-load 'compile
  (push '(my-linter
          "^\\[\\(ERROR\\|WARN\\)\\] \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
          2 3 4 (1 . nil))
        compilation-error-regexp-alist-alist)
  (push 'my-linter compilation-error-regexp-alist))
```

The TYPE field `(1 . nil)` means: "if group 1 matches, it's a warning" -- but
wait, group 1 always matches. The trick is that compilation mode checks the
*content* of the match. Actually, let me correct myself. The TYPE field
should be a number or expression. A cleaner approach:

```emacs-lisp
(with-eval-after-load 'compile
  (push '(my-linter
          "^\\[\\(?:ERROR\\|\\(WARN\\)\\)\\] \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
          2 3 4 (1))
        compilation-error-regexp-alist-alist)
  (push 'my-linter compilation-error-regexp-alist))
```

Here group 1 only matches for `WARN` lines (it's inside a non-capturing group
with an alternative). TYPE is `(1)` meaning "if group 1 matched, it's a
warning; otherwise it's an error."

Now `M-x compile` with your linter command will highlight errors and warnings
differently, and `next-error` will jump right to them.

## Useful Variables You Might Not Know

A few compilation variables that are worth knowing:

```emacs-lisp
;; OCaml (and some other languages) use 0-indexed columns
(setq-local compilation-first-column 0)

;; Scroll the compilation buffer to follow output
(setq compilation-scroll-output t)

;; ... or scroll until the first error appears
(setq compilation-scroll-output 'first-error)

;; Skip warnings and info when navigating with next-error
(setq compilation-skip-threshold 2)

;; Auto-close the compilation window on success
(setq compilation-finish-functions
      (list (lambda (buf status)
              (when (string-match-p "finished" status)
                (run-at-time 1 nil #'delete-windows-on buf)))))
```

The `compilation-skip-threshold` is particularly useful. Set it to 2 and
`next-error` will only stop at actual errors, skipping warnings and info
messages. Set it to 1 to also stop at warnings but skip info. Set it to 0 to
stop at everything.

## The Compilation Mode Family

Compilation mode isn't just for compilers. Several built-in modes derive from it:

- `grep-mode` -- `M-x grep`, `M-x rgrep`, `M-x lgrep` all produce output
  in a compilation-derived buffer. Same `next-error` navigation, same
  keybindings.
- `occur-mode` -- `M-x occur` isn't technically derived from compilation
  mode, but it participates in the same `next-error` infrastructure.
- `flymake`/`flycheck` -- uses compilation-style error navigation under the hood.

The `grep` family deserves special mention. `M-x rgrep` is recursive grep with
file-type filtering, and it's surprisingly powerful for a built-in tool. The
results buffer supports all the same navigation, plus `M-x wgrep` (from the
[wgrep](https://github.com/mhayashi1120/Emacs-wgrep) package) lets you *edit*
grep results and write the changes back to the original files. That's a workflow
that rivals any modern IDE.

## Building a Derived Mode

The real fun begins when you create your own compilation-derived mode. Let's
build one for running [RuboCop](https://github.com/rubocop/rubocop) (a Ruby
linter and formatter). RuboCop's `emacs` output format looks like this:

```
app/models/user.rb:10:5: C: Style/StringLiterals: Prefer single-quoted strings
app/models/user.rb:25:3: W: Lint/UselessAssignment: Useless assignment to variable - x
app/models/user.rb:42:1: E: Naming/MethodName: Use snake_case for method names
```

The format is `FILE:LINE:COLUMN: SEVERITY: CopName: Message` where severity is
`C` (convention), `W` (warning), `E` (error), or `F` (fatal).

Here's a complete derived mode:

```emacs-lisp
(require 'compile)

(defvar rubocop-error-regexp-alist
  `((rubocop-offense
     ;; file:line:col: S: Cop/Name: message
     "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\([EWFC]\\)\\): "
     1 2 3 (5 . nil)
     nil (4 compilation-warning-face)))
  "Error regexp alist for RuboCop output.
Group 5 captures the severity letter: E/F = error, W/C = warning.")

(define-compilation-mode rubocop-mode "RuboCop"
  "Major mode for RuboCop output."
  (setq-local compilation-error-regexp-alist
              (mapcar #'car rubocop-error-regexp-alist))
  (setq-local compilation-error-regexp-alist-alist
              rubocop-error-regexp-alist))

(defun rubocop-run (&optional directory)
  "Run RuboCop on DIRECTORY (defaults to project root)."
  (interactive)
  (let ((default-directory (or directory (project-root (project-current t)))))
    (compilation-start "rubocop --format emacs" #'rubocop-mode)))
```

A few things to note:

- `define-compilation-mode` creates a major mode derived from
  `compilation-mode`. It inherits all the navigation, font-locking, and
  `next-error` integration for free.
- We set `compilation-error-regexp-alist` and
  `compilation-error-regexp-alist-alist` as buffer-local. This means our mode
  only uses *its own* regexps, not the global ones. No interference with other
  tools.
- `compilation-start` is the workhorse -- it runs the command and displays
  output in a buffer using our mode.
- The TYPE field `(5 . nil)` means: if group 5 matched, check its content --
  but actually, here all lines match group 5. The subtlety is that compilation
  mode treats a non-nil TYPE group as a warning. To distinguish E/F from W/C,
  you'd need a predicate or two separate regexp entries. For simplicity, this
  version treats everything as an error, which is usually fine for a linter.

You could extend this with auto-fix support (`rubocop -A`), or a sentinel
function that sends a notification when the run finishes:

```emacs-lisp
(defun rubocop-run (&optional directory)
  "Run RuboCop on DIRECTORY (defaults to project root)."
  (interactive)
  (let ((default-directory (or directory (project-root (project-current t))))
        (compilation-finish-functions
         (cons (lambda (_buf status)
                 (message "RuboCop %s" (string-trim status)))
               compilation-finish-functions)))
    (compilation-start "rubocop --format emacs" #'rubocop-mode)))
```

**Side note:** RuboCop actually ships with a built-in `emacs` output formatter
(that's what `--format emacs` uses above), so its output already matches
Emacs's default compilation regexps out of the box -- no custom mode needed. I
used it here purely to illustrate how `define-compilation-mode` works. In
practice you'd just `M-x compile RET rubocop --format emacs` and everything
would Just Work.[^1]

[^1]: Full disclosure: I may know a thing or two about RuboCop's Emacs formatter.

## next-error is not really an error

> There is no spoon.
>
> -- The Matrix

The most powerful insight about compilation mode is that it's not really about
compilation. It's about **structured output with source locations**. Any tool
that produces file/line references can plug into this infrastructure, and once it
does, you get `next-error` navigation for free. The name `compilation-mode` is a
bit of a misnomer -- something like `structured-output-mode` would be more
accurate. But then again, naming is hard, and this one has 30+ years of momentum
behind it.

This is one of Emacs's great architectural wins. Whether you're navigating
compiler errors, grep results, test failures, or linter output, the workflow is
the same: `M-g n` to jump to the next problem. Once your fingers learn that
pattern, it works everywhere.

I used `M-x compile` for two decades before I really understood the machinery
underneath. Sometimes the tools you use every day are the ones most worth
revisiting.

That's all I have for you today. In Emacs we trust!
