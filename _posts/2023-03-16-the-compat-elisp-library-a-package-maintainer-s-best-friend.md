---
layout: post
title: 'The Compat Elisp Library: A Package Maintainer''s Best Friend'
date: 2023-03-16 09:18 +0200
tags:
- Forward Compatibility
- Package Maintenance
- Libraries
---

I guess every package maintainer has been in the following situation - an upcoming version of Emacs introduces some new cool API that you'd like to use immediately. Or simply you have to maintain compatibility with many old Emacs versions, but you'd still like to use some modern APIs. Usually what happens in this case is that people start to backport themselves whatever Elisp APIs they need. E.g. for a long time CIDER had a file named `cider-compat.el` that looked something like this right before it became obsolete:

``` emacs-lisp
(eval-and-compile

  (unless (fboundp 'if-let*)
    (defmacro if-let* (bindings then &rest else)
      "Process BINDINGS and if all values are non-nil eval THEN, else ELSE.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in THEN, and its cadr is a sexp to be
evalled to set symbol's value."
      (declare (indent 2)
               (debug ([&or (&rest (symbolp form)) (symbolp form)] form body)))
      `(let* ,(internal--build-bindings bindings)
         (if ,(car (internal--listify (car (last bindings))))
             ,then
           ,@else))))

  (unless (fboundp 'when-let*)
    (defmacro when-let* (bindings &rest body)
      "Process BINDINGS and if all values are non-nil eval BODY.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in BODY, and its cadr is a sexp to be
evalled to set symbol's value."
      (declare (indent 1) (debug if-let*))
      `(if-let* ,bindings ,(macroexp-progn body)))))

(provide 'cider-compat)
```

I've done something similar for many packages and I've seen it in the wild countless times. But there is a better and simpler way to get access to those newer APIs - enter the [compat](https://elpa.gnu.org/packages/compat.html) library.

> `Compat` is the Elisp forwards compatibility library, which provides
> definitions introduced in newer Emacs versions.[^1]  The definitions
> are only installed if necessary for your current Emacs version.  If
> Compat is compiled on a recent version of Emacs, all of the
> definitions are disabled at compile time, such that no negative
> performance impact is incurred.  The provided compatibility
> implementations of functions and macros are at least subsets of the
> actual implementations.  Be sure to read the documentation string
> and [the Compat manual](https://elpa.gnu.org/packages/doc/compat.html).
>
> Not every function provided in newer versions of Emacs is provided
> here.  Some depend on new features from the C core, others cannot
> be implemented to a meaningful degree.  Please consult the Compat
> manual for details regarding the usage of the Compat library and
> the provided functionality.
>
> The main audience for this library are not regular users, but
> package maintainers.  Therefore no commands, user-facing modes or
> user options are implemented here.

The above description is taken verbatim from the package and I don't really have
much to add to it. Here's also what one of the package's maintainers has to say about it
and its use-cases:[^2]

> Over time Emacs has seen useful standard library additions, for example
> additional `string-*` functions or the new `keymap-*` functions in
> Emacs 29. Compat provides many of these new functions and closes the gap
> between Emacs core development and package development outside of Emacs, since
> new Emacs additions become available early for all package developers.
>
> Packages outside of Emacs core can be written in the same style as Emacs core
> packages by relying on Compat. Vice versa Emacs core packages can use Compat
> for their separate GNU ELPA releases, as is done for example by ERC, the Emacs
> IRC client. Compat should make it easy to move packages out of core or into
> the core, while still allowing separate GNU ELPA releases. Using Compat may
> increase the performance of your packages, since you can use optimized
> functions like ntake, which will only fallback to a slower compatibility
> version on older Emacs versions.

In a nutshell `compat` bridges the gap between what's available for built-in Emacs
packages and third-party (external) packages. And this is totally awesome!

I can recommend checking out the [source
code](https://github.com/emacs-compat/compat) of the library and its extensive
[changelog](https://github.com/emacs-compat/compat/blob/master/NEWS.org). You'll
notice how internally the code is organized in files matching various Emacs
versions (e.g. `compat-25.el`, `compat-26.el`, etc) and that the library makes
heavy use of custom macros like `compat-defun`, `compat-defalias` and
`compat-defmacro` (all defined in `compat-macs.el`) for the backported APIs. Here are a few examples from `compat-29.el`:

``` emacs-lisp
(compat-defun list-of-strings-p (object) ;; <compat-tests:lists-of-strings-p>
  "Return t if OBJECT is nil or a list of strings."
  (declare (pure t) (side-effect-free t))
  (while (and (consp object) (stringp (car object)))
    (setq object (cdr object)))
  (null object))

(compat-defun plistp (object) ;; <compat-tests:plistp>
  "Non-nil if and only if OBJECT is a valid plist."
  (let ((len (proper-list-p object)))
    (and len (zerop (% len 2)))))

(compat-defun delete-line () ;; <compat-tests:delete-line>
  "Delete the current line."
  (delete-region (pos-bol) (pos-bol 2)))

(compat-defmacro with-restriction (start end &rest rest) ;; <compat-tests:with-restriction>
  "Execute BODY with restrictions set to START and END.
The current restrictions, if any, are restored upon return.
When the optional :label LABEL argument is present, in which
LABEL is a symbol, inside BODY, `narrow-to-region' and `widen'
can be used only within the START and END limits.  To gain access
to other portions of the buffer, use `without-restriction' with the
same LABEL argument.
\(fn START END [:label LABEL] BODY)"
  (declare (indent 0) (debug t))
  `(save-restriction
     (narrow-to-region ,start ,end)
     ;; Locking is ignored
     ,@(if (eq (car rest) :label) (cddr rest) rest)))
```

And that's a wrap. I think pretty much every package maintainer can benefit from
this library in their packages (unless they have aversion to external
dependencies that is). I have to admit that I learned about its existence only
recently and I can't believe I missed something so useful for so long. Mistake
corrected! Keep hacking!

[^1]: "Newer" here means means Emacs 25+. Compat itself supports Emacs 24.4+.
[^2]: Taken from <https://www.reddit.com/r/emacs/comments/10iep0o/compat_29130>
