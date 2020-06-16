---
layout: post
title: 'Weird Emacs: Listy Lambdas'
date: 2020-06-16 23:12 +0300
tags:
- Emacs Lisp
- Weird Emacs
---

I guess everyone who has spent any time with Emacs Lisp, knows that the language
is full of oddities and quirks. That's often a source of frustration for people
coming from other languages, but it's part of Emacs's charm. Even after 15 years
with Emacs and working on numerous Emacs packages, I still find things that
manage to surprise me in Emacs Lisp. Consider the following:

``` emacs-lisp
(functionp (lambda (x) "I don't do much")) ;; => t
(functionp '(1 2 3)) ;; => nil

(listp (lambda (x) "I don't do much")) ;; => t
(listp '(1 2 3)) ;; => t
```

Turns out lambdas are both functions and lists! Weird, right?

I learned this yesterday, when someone reported a [bug in Projectile](https://github.com/bbatsov/projectile/pull/1545) triggered
by the following code:

``` emacs-lisp
(if (listp marker)
    (and (projectile-verify-files marker) project-type)
  (and (funcall marker) project-type))
```

`marker` could be both a function or a list and this `if` covers both cases.
It works fine if `marker` is something defined with `defun`, but blows up for lambdas,
as `listp` returns `t` for them. The solution to the bug was pretty simple:

``` emacs-lisp
(if (functionp marker)
    (and (funcall marker) project-type)
  (and (projectile-verify-files marker) project-type))
```

I guess that's the lesson from today's post - if something can be both a lambda and list, you definitely
want to have the `functionp` check first.

That's all I have for you today. Keep hacking!

**P.S.** Feel free to share your favourite weird things about Emacs Lisp in the comments!
