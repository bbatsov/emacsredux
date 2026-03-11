---
layout: post
title: Code Formatting in Emacs
date: 2026-03-11 10:30 +0200
tags:
- Editing
- Configuration
---

I got inspired to look into this topic after receiving the following obscure bug
report for [neocaml](https://github.com/bbatsov/neocaml):

> I had your package installed. First impressions were good, but then I had to
> uninstall it. The code formatting on save stopped working for some reason, and
> the quickest solution was to revert to the previous setup.

I found this somewhat entertaining -- neocaml is a major mode, it has nothing to
do with code formatting. But I still started to wonder what kind of setup that
user might have. So here we are!

Code formatting is one of those things that shouldn't require much thought --
you pick a formatter, you run it, and your code looks consistent. In practice,
Emacs gives you a surprising number of ways to get there, from built-in
indentation commands to external formatters to LSP-powered solutions. This
article covers the landscape and helps you pick the right approach.

One thing to note upfront: most formatting solutions hook into saving the buffer,
but there are two distinct patterns. The more common one is *format-then-save*
(via `before-save-hook`) -- the buffer is formatted before it's written to disk,
so the file is always in a formatted state. The alternative is *save-then-format*
(via `after-save-hook`) -- the file is saved first, then formatted and saved
again. The second approach can be done asynchronously (the editor doesn't block),
but it means the file is briefly unformatted on disk. Keep this distinction in
mind as we go through the options.

## Built-in: Indentation, Not Formatting

Let's get the terminology straight. Emacs has excellent built-in *indentation*
support, but indentation is not the same as formatting. `indent-region` (`C-M-\`)
adjusts leading whitespace according to the major mode's rules. It won't
reformat long lines, reorganize imports, add or remove blank lines, or apply any
of the opinionated style choices that modern formatters handle.

That said, for many languages (especially Lisps), `indent-region` on the whole
buffer is all the formatting you'll ever need:

```emacs-lisp
;; A simple indent-buffer command (Emacs doesn't ship one)
(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
```

**Tip:** `whitespace-cleanup` is a nice complement -- it handles trailing
whitespace, mixed tabs/spaces, and empty lines at the beginning and end of the
buffer. Adding it to `before-save-hook` keeps things tidy:

```emacs-lisp
(add-hook 'before-save-hook #'whitespace-cleanup)
```

## Shelling Out: The DIY Approach

The simplest way to run an external formatter is `shell-command-on-region`
(`M-|`). With a prefix argument (`C-u M-|`), it replaces the region with the
command's output:

```
C-u M-| prettier --stdin-filepath foo.js RET
```

(The `--stdin-filepath` flag doesn't read from a file -- it just tells Prettier
which parser to use based on the filename extension.)

You can wrap this in a command for repeated use:

```emacs-lisp
(defun format-with-prettier ()
  "Format the current buffer with Prettier."
  (interactive)
  (let ((point (point)))
    (shell-command-on-region
     (point-min) (point-max)
     "prettier --stdin-filepath buffer.js"
     (current-buffer) t)
    (goto-char point)))
```

This works, but it's fragile -- no error handling, no automatic file type
detection, and cursor position is only approximately preserved. For anything
beyond a quick one-off, you'll want a proper package.

## reformatter.el: Define Your Own (Re)Formatters

[reformatter.el](https://github.com/purcell/emacs-reformatter) is a small
library that generates formatter commands from a simple declaration. You define
the formatter once, and it creates everything you need:

```emacs-lisp
(reformatter-define black-format
  :program "black"
  :args '("-q" "-")
  :lighter " Black")
```

This single form generates three things:

- `black-format-buffer` -- format the entire buffer
- `black-format-region` -- format the selected region
- `black-format-on-save-mode` -- a minor mode that formats on save

Enabling format-on-save is then just:

```emacs-lisp
(add-hook 'python-mode-hook #'black-format-on-save-mode)
```

`reformatter.el` handles temp files, error reporting, and stdin/stdout piping.
It also supports formatters that work on files instead of stdin (via `:stdin nil`
and `:input-file`), and you can use buffer-local variables in `:program` and
`:args` for per-project configuration via `.dir-locals.el`.

I love the approach taken by this package! It's explicit, you
see exactly what's being called, and the generated on-save mode plays nicely
with the rest of your config.

## format-all: Zero Configuration

[format-all](https://github.com/lassik/emacs-format-all-the-code) takes a
different approach -- it auto-detects the right formatter for 70+ languages
based on the major mode. You don't define anything; it just works:

```emacs-lisp
(add-hook 'prog-mode-hook #'format-all-mode)
(add-hook 'prog-mode-hook #'format-all-ensure-formatter)
```

The main command is `format-all-region-or-buffer`. The `format-all-mode` minor
mode handles format-on-save. If you need to override the auto-detected formatter,
set `format-all-formatters` (works well in `.dir-locals.el`):

```emacs-lisp
;; In .dir-locals.el -- use black instead of autopep8 for Python
((python-mode . ((format-all-formatters . (("Python" black))))))
```

```emacs-lisp
;; Or in your init file
(setq-default format-all-formatters '(("Python" black)))
```

The trade-off is less control -- you're trusting the package's formatter
database, and debugging issues is harder when you don't see the underlying
command.

## apheleia: Async and Cursor-Aware

[apheleia](https://github.com/radian-software/apheleia) is the most
sophisticated option. It solves two problems the other packages don't:

1. **Asynchronous formatting** -- it runs the formatter *after* save, so the
   editor never blocks. If you modify the buffer before formatting completes,
   the result is discarded.
2. **Cursor preservation** -- instead of replacing the entire buffer, it applies
   changes as [RCS](https://en.wikipedia.org/wiki/Revision_Control_System)
   patches (a classic diff format from one of the earliest version control
   systems), so your cursor position and scroll state are maintained.

```emacs-lisp
;; Enable globally
(apheleia-global-mode +1)
```

apheleia auto-detects formatters like `format-all`, but you can configure things
explicitly:

```emacs-lisp
;; Chain multiple formatters (e.g., sort imports, then format)
(setf (alist-get 'python-mode apheleia-mode-alist)
      '(isort black))
```

Formatter chaining is a killer feature -- `isort` then `black`, `eslint` then
`prettier`, etc. No other package handles this as cleanly.

**Caveat:** Because apheleia formats after save, the file on disk is briefly in
an unformatted state. This is usually fine, but it can confuse tools that watch
files for changes. It also doesn't support TRAMP/remote files.

## LSP: eglot and lsp-mode

If you're already using a language server, formatting is built in. The language
server handles the formatting logic, and Emacs just sends the request.

### eglot (built-in since Emacs 29)

```emacs-lisp
;; Manual formatting
M-x eglot-format          ;; formats region or buffer
M-x eglot-format-buffer   ;; formats the entire buffer
```

Format-on-save requires a hook -- eglot doesn't provide a toggle for it:

```emacs-lisp
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))
```

The `nil t` makes the hook buffer-local, so it only fires in eglot-managed
buffers.

### lsp-mode

```emacs-lisp
;; Manual formatting
M-x lsp-format-buffer
M-x lsp-format-region
```

lsp-mode has a built-in option for format-on-save:

```emacs-lisp
(setq lsp-format-buffer-on-save t)
```

It also supports on-type formatting (formatting as you type closing braces,
semicolons, etc.) via `lsp-enable-on-type-formatting`, which is enabled by
default.

### LSP Caveats

- Formatting capabilities depend entirely on the language server. Some servers
  (like `gopls` or `rust-analyzer`) have excellent formatters; others may not
  support formatting at all.
- The formatter's configuration lives outside Emacs -- in `.clang-format`,
  `pyproject.toml`, `.prettierrc`, etc. This is actually a feature if you're
  working on a team, since the config is shared.
- LSP formatting can be slow for large files since it's a round-trip to the
  server process.

## Which Approach Should You Use?

There's no single right answer, but here's a rough guide:

- **Lisps and simple indentation needs:** Built-in `indent-region` is probably
  all you need.
- **Specific formatter, full control:** `reformatter.el` -- explicit, simple,
  and predictable.
- **Many languages, minimal config:** `format-all` or `apheleia`. Pick
  `apheleia` if you want async formatting and cursor stability.
- **Already using LSP:** Just use `eglot-format` / `lsp-format-buffer`. One less
  package to maintain.
- **Mixed setup:** Nothing stops you from using LSP formatting for some languages
  and `reformatter.el` for others. Just be careful not to have two things
  fighting over format-on-save for the same mode.

**Tip:** Whichever approach you choose, consider enabling format-on-save per
project via `.dir-locals.el` rather than globally. Not every project uses the
same formatter (or any formatter at all), and formatting someone else's
unformatted codebase on save is a recipe for noisy diffs.

```emacs-lisp
;; .dir-locals.el
((python-mode . ((eval . (black-format-on-save-mode)))))
```


## Epilogue

So many options, right? That's so Emacs!

I'll admit that I don't actually use any of the packages mentioned in this
article -- I learned about all of them while doing a bit of research for
alternatives to the DIY and LSP approaches. That said, I have a very high
opinion of everything done by
[Steve Purcell](https://github.com/purcell) (author of `reformatter.el` and
the popular [Emacs Prelude-like config](https://github.com/purcell/emacs.d)) and
[Radon Rosborough](https://github.com/radian-software) (author of `apheleia`,
`straight.el`, and the [Radian](https://github.com/radian-software/radian) Emacs
config), so I have no issue endorsing packages created by them.

I'm in camp LSP most of the time these days, and I'd guess most people are too.
But if I weren't, I'd probably take `apheleia` for a spin. Either way, it's
never bad to have options, right? There are languages where LSP isn't as prevalent -- all
sorts of Lisp dialects, for instance -- where something like `apheleia` or
`reformatter.el` might come in handy. But then again, in Lisps `indent-region`
works so well that you rarely need anything else. I'm a huge fan of
`indent-region` myself -- for any good Emacs mode, it's all the formatting you
need.
