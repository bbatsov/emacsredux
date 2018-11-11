---
layout: post
title: "Boost Performance by Leveraging Byte-compilation"
date: 2013-06-25 12:49
comments: true
tags:
- Utilities
---

Emacs's Lisp interpreter is able to interpret two kinds of code:
humanly readable code (stored in files with `.el` extension) and
machine optimized code (called `byte-compiled code`), which is not
humanly readable. Byte-compiled code runs faster than humanly readable
code. Java or .NET developers should already be familiar with the
concept of byte-code, since it's pretty central on those platforms.

You can transform humanly readable code into byte-compiled code by
running one of the compile commands such as `byte-compile-file`. The
resulting byte-code is stored into `.elc` files. One can also
byte-compile Emacs Lisp source files using Emacs in batch mode.

Here's how you can compile everything in your `.emacs.d` folder:

```
emacs -batch -f batch-byte-compile ~/.emacs.d/**/*.el
```

Of course we can easily create an Emacs command that does the same thing:

``` elisp
(defun er-byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))
```

`user-emacs-directory` is an Emacs variable that points to your init
dir (usually `~/.emacs.d` on UNIX systems). This command will
recompile even files that were already compiled before (meaning a file
with the same name and the `.elc` extension instead of `.el`
existed). You can try the new command with `M-x
byte-compile-init-dir`.

You have to keep in mind that Emacs will load code from the `.elc`
files if present alongside the `.el` files, so you'll have to take
steps to ensure you don't have stale `.elc` files lying around. I'd
suggest the following solution:

``` elisp
(defun er-remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'er-remove-elc-on-save)
```

This code will make Emacs delete the `some_file.elc` file, every time the
`some_file.el` file in the same folder is saved.

A couple of closing notes:

* If you don't have any custom computationally intensive `defuns` in
your init directory - it probably doesn't make sense to byte-compile
it.
* Packages installed via `package.el` will be automatically
  byte-compiled during the installation process.

The code presented here is part of
[Prelude](https://github.com/bbatsov/prelude). As a matter of fact
Prelude will byte-compile itself during the installation process (if
you used the installation script, that is). Prelude will also recompile
itself when `M-x prelude-update` is invoked.
