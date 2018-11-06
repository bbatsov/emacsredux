---
layout: post
title: "Highlight comment annotations"
date: 2013-07-24 16:15
comments: true
tags:
- Utilities
---

Programming code is often filled with comment annotations indicating stuff that should be done in the future.


``` ruby
# REFACTOR: Decouple and clean up this crap.

# crappy code omitted
```

Normally Emacs doesn't highlight such comment annotations, unless
you're using some minor mode like
[fic-mode](https://github.com/lewang/fic-mode/blob/master/fic-mode.el). I find such mode overkill given the fact we can cook a pretty decent solution in just about 5 lines of code:

``` elisp
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)
```

And that's all there is to it. This code is not perfect, since it
would highlight `FIXME:` everywhere in the source code (as opposed to
only in comments), but it's extremely highly unlikely that it'll
appear outside of the source comments anyways.

As usual [Prelude](https://github.com/bbatsov/prelude) users get this
functionally for free out-of-the-box.
