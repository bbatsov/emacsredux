---
layout: post
title: EditorConfig Support is Now Built-in
date: 2026-02-28 10:00 +0200
tags:
- Emacs 30
- Built-ins
---

[EditorConfig](https://editorconfig.org/) is one of those simple ideas that just
works. Drop a `.editorconfig` file in your project root, and editors that support
it will automatically pick up the right indentation style, tab width, line
endings, and other formatting settings. It's particularly useful when you work
across multiple projects with different coding conventions — or when you
collaborate with people who use different editors.

Emacs users have had access to EditorConfig support via the third-party
`editorconfig` package for years. Starting with Emacs 30, it's **built-in**.

## Enabling it

```emacs-lisp
(editorconfig-mode 1)
```

That's all you need. Once enabled, Emacs will automatically detect and apply
settings from `.editorconfig` files whenever you open a file in a project that
has one.

## What does it support?

A typical `.editorconfig` file looks like this:

```ini
root = true

[*]
indent_style = space
indent_size = 4
end_of_line = lf
charset = utf-8
trim_trailing_whitespace = true
insert_final_newline = true

[*.md]
trim_trailing_whitespace = false

[Makefile]
indent_style = tab
```

When `editorconfig-mode` reads this, it sets the appropriate Emacs variables
(`indent-tabs-mode`, `tab-width`, `require-final-newline`, etc.) for the current
buffer.

## Editing .editorconfig files

Emacs 30 also includes `editorconfig-conf-mode`, a major mode for editing
`.editorconfig` files themselves. It provides syntax highlighting and basic
editing support. This mode is automatically associated with `.editorconfig`
files, so it should just work.

## Replacing the third-party package

If you're already using the `editorconfig` package from MELPA, you should be able to
simply remove it and rely on the built-in version. The built-in package is
based on the same codebase, so the transition should be seamless.

That's all I have for you today. Keep hacking!
