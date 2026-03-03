---
layout: post
title: Emacs 30 Comes with a Built-in JSON Parser
date: 2026-02-28 09:30 +0200
tags:
- Emacs 30
---

Here's a change in Emacs 30 that might not sound exciting at first, but has
significant implications for performance: Emacs now ships with a **built-in JSON
parser** and no longer depends on the external `libjansson` C library.

## What changed

Prior to Emacs 30, native JSON support (the `json-serialize`, `json-parse-string`, etc.
family of functions) required Emacs to be compiled with `libjansson`. If the library
wasn't available, Emacs would fall back to a much slower pure Elisp JSON
implementation. Many Linux distributions included `libjansson`, but it was an
extra dependency that wasn't always present.

In Emacs 30:

- JSON support is **always available** — no external library needed.
- `json-available-p` now always returns `t` (kept only for backward compatibility).
- The `--with-json` configure flag has been removed entirely.

## Why it matters

This matters most for anything that does heavy JSON processing — and in modern
Emacs, that's primarily **LSP**. Both `eglot` and `lsp-mode` communicate with
language servers over JSON-RPC, which means they're constantly parsing and
serializing JSON. A faster JSON implementation translates directly into a
snappier LSP experience.

The new built-in parser eliminates the overhead of the FFI (Foreign Function
Interface) calls to `libjansson` and reduces memory allocations, which results in
noticeably faster performance.

## One breaking change to watch for

`json-serialize` now returns a **unibyte string** instead of a multibyte string. This
is technically more correct (JSON is an encoding format, and the output is
UTF-8 bytes), but if your code assumed a multibyte result, you might need to
adjust:

```emacs-lisp
;; If you need a multibyte string for some reason:
(decode-coding-string (json-serialize obj) 'utf-8)
```

In practice, this is unlikely to affect most users, but package authors should
take note.

That's all I have for you today. Keep hacking!
