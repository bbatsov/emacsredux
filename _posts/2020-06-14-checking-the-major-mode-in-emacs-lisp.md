---
layout: post
title: Checking the Major Mode in Emacs Lisp
date: 2020-06-14 14:28 +0300
tags:
- Emacs Lisp
---

Often when working on some Emacs package you'd want to create some logic
that's conditional on the major mode of a particular Emacs buffer.
There are several ways check the major mode, but some are definitely better than others.

Every buffer has a buffer-local variable named `major-mode` that you can check
directly.  If you evaluate the symbol `major-mode` in a scratch buffer or an
Emacs Lisp REPL (`M-x ielm`), you'll get `list-interaction-mode` and
`inferior-emacs-lisp-mode`. You can easily do this for any buffer by pressing
`M-:` (`M-x eval-expression`) and typing `major-mode` in the minibuffer when
prompted to do so. Making this a bit more generic can easily obtain the major
mode of any buffer like this:

``` emacs-lisp
;; option 1
(with-current-buffer buffer
  major-mode)

;; option 2
(buffer-local-value 'major-mode buffer)
```

So, how would you compare `major-buffer` to something? As it's a symbol, the first thing that comes to mind is using `eq`:

``` emacs-lisp
(if (eq major-mode 'clojure-mode)
    (do-something))
```

While this generally works, there's one subtle problem with it - you're doing an
exact match for a particular mode, but major modes can be inherited by other
modes. Consider `clojure-mode` - it's the parent of modes like
`clojurescript-mode` and `clojurec-mode`, and it inherits from `prog-mode`
(which is the parent mode of most programming major modes). Enter
`derived-mode-p`:

``` emacs-lisp
;; assuming we're in a ClojureScript buffer and the current major mode is clojurescript-mode
(derived-mode-p 'clojurescript-mode)
;; => t

(derived-mode-p 'clojure-mode)
;; => t

(derived-mode-p 'prog-mode)
;; => t
```

As you can see from the examples above, `derived-mode-p` understands the major mode inheritance hierarchy, which makes it the
best solution for most cases when you'd want to do something depending on the major mode. Unfortunately I've seen
too many times `eq` used when `derived-mode-p` would be a better option, which is why I decided to write this short
article.

That's all I've had for you today! Keep hacking!
