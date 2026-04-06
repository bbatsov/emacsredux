---
layout: post
title: "The Many Faces of flet: cl-flet, cl-labels, and cl-letf"
date: 2026-04-06 15:00 +0300
tags:
- Emacs Lisp
- cl-lib
---

Way back in 2013 I wrote about [the deprecation of
`flet`](/blog/2013/09/05/a-proper-replacement-for-flet/) and how
`noflet` could fill the gap. Thirteen years later, it's probably time
for a proper overview of what replaced `flet` in `cl-lib` and when to
use each option.

Emacs Lisp doesn't have a built-in way to define local functions (the
way `let` defines local variables), so `cl-lib` provides several macros
for this. If you've ever been confused by `cl-flet`, `cl-labels`, and
`cl-letf` -- you're not alone. The naming doesn't make the distinctions
obvious, and the documentation is a bit dry. Let's try to fix that.

<!--more-->

## A Bit of History

The original `flet` (from the old `cl` package) let you temporarily
override a function's definition. It worked by swapping out the
function's `symbol-function` cell and restoring it when the body
finished -- essentially a dynamic `let` for functions:

``` emacs-lisp
;; Old-style flet (deprecated since Emacs 24.3)
(flet ((some-function () "overridden result"))
  ;; Everything here, including called functions, sees the override
  (some-function))
```

This was very handy for testing (stubbing impure functions), but it
conflated two different things into one macro:

1. Defining local helper functions (a lexical concept)
2. Temporarily overriding a global function (a dynamic concept)

When `cl` was reorganized into `cl-lib` in Emacs 24.3, `flet` was
split into separate macros for each use case. This also brought the
lexical variants in line with Common Lisp semantics, where `flet` and
`labels` are lexically scoped.

## The Three Replacements

### cl-flet: Local Functions (No Recursion)

`cl-flet` binds function names lexically within its body. The key
thing to understand is that the binding is only visible in the body
forms -- not inside the function's own definition, and not to any
functions you call:

``` emacs-lisp
(cl-flet ((double (n) (* n 2)))
  (double 21)) ; => 42
```

Because it's lexical, `cl-flet` cannot override functions seen by
other code:

``` emacs-lisp
(defun my-helper () (+ 1 2))
(defun my-caller () (my-helper))

(cl-flet ((my-helper () 999))
  (my-caller))  ; => 3, NOT 999!
```

`my-caller` still sees the original `my-helper`. This is the
fundamental difference from the old `flet`.

There's also `cl-flet*`, which is to `cl-flet` what `let*` is to
`let` -- each binding can reference the ones before it.

Use `cl-flet` when you just need a simple local helper and don't need
recursion. Think of it as `let` for functions.

### cl-labels: Local Functions (With Recursion)

`cl-labels` is like `cl-flet`, but the function *is* visible inside
its own body and inside the bodies of sibling bindings. This makes
recursion and mutual recursion possible:

``` emacs-lisp
(cl-labels ((factorial (n)
              (if (<= n 1) 1
                (* n (factorial (- n 1))))))
  (factorial 10)) ; => 3628800
```

This would blow up with `cl-flet` because `factorial` wouldn't be
defined inside its own body.

Mutual recursion works too:

``` emacs-lisp
(cl-labels ((my-even-p (n) (if (= n 0) t (my-odd-p (- n 1))))
            (my-odd-p (n) (if (= n 0) nil (my-even-p (- n 1)))))
  (list (my-even-p 4) (my-odd-p 3))) ; => (t t)
```

Use `cl-labels` when your local functions need to call themselves or
each other.

**Note:** `cl-labels` requires `lexical-binding` to be `t` in the file (which
it really should be for any modern Emacs Lisp code).

### cl-letf: Temporary Global Override

This is the one that actually replaces the old `flet`'s dynamic
behavior. `cl-letf` temporarily rebinds a generalized place (anything
`setf` can handle) and restores it on exit:

``` emacs-lisp
(defun my-helper () (+ 1 2))
(defun my-caller () (my-helper))

(cl-letf (((symbol-function 'my-helper) (lambda () 999)))
  (my-caller))  ; => 999
```

Now `my-caller` *does* see the override, because `cl-letf` modifies the
actual `symbol-function` cell of `my-helper` -- just like the old `flet` did.
The original definition is restored when the body exits, even on error.

The syntax is a bit verbose because `cl-letf` isn't specific to
functions -- it's a general-purpose temporary binding macro for any
`setf`-able place. `(symbol-function 'name)` is a "generalized
variable" that refers to the function stored in a symbol's function
cell -- it's just one of many places `cl-letf` can bind. For example:

``` emacs-lisp
;; Temporarily silence messages
(cl-letf (((symbol-function 'message) #'ignore))
  (do-something-noisy))
```

Use `cl-letf` when you need the old dynamic `flet` behavior -- typically
for testing (stubbing functions) or temporarily suppressing/redirecting
behavior.

## Quick Reference

| | Scope | Recursion | Overrides global? |
|---|---|---|---|
| `cl-flet` | Lexical | No | No |
| `cl-labels` | Lexical | Yes | No |
| `cl-letf` | Dynamic | N/A | Yes |

In other words:

- Default to `cl-flet` for local helpers. It's the simplest and
  most predictable.
- Reach for `cl-labels` when you need recursion or mutual
  recursion in local functions.
- Use `cl-letf` only when you genuinely need dynamic override --
  mainly in tests. Modifying global function cells is a sharp tool and
  it's not thread-safe, so keep it contained.

---

That's all I have for you today. Keep hacking!
