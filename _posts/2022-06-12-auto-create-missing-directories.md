---
layout: post
title: Auto-create Missing Directories
date: 2022-06-12 11:02 +0300
tags:
- Utilities
---

Every now and then I'd do something like `C-x C-f
/path/to/new/dir/some-file.el`.  As the new folder doesn't exist Emacs will
prompt me to press `RET` twice to create it. It's not a big deal, but it always
annoys me a little bit and I'm always looking for ways to optimize my workflow.

At first I thought I'd have to advise `find-file` and just add some check in the advice whether the
destination folder exists. Here's how this typically works:

``` emacs-lisp
(defun er-auto-create-missing-dirs (orig-fun &rest args)
  (let* ((filename (car args))
         (target-dir (file-name-directory filename)))
    (unless (file-directory-p directory)
      (make-directory directory t))
    (apply orig-fun arg)))

(advice-add 'find-file :around 'er-auto-create-missing-dirs)
```

That gets the job done, but I've never been super fond of using advices as they
add some complexity to the process of debugging something.
Turns out there's a much simpler solution, though - the hook `find-file-not-found-functions`.
According to the docs:

> List of functions to be called for `find-file` on nonexistent file. These functions are called as soon as the error is detected. Variable `buffer-file-name` is already set up. The functions are called in the order given until one of them returns non-nil.

This means all we need to do is write a simple hook function:

``` emacs-lisp
(defun er-auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)
```

Basically we just need to extract the destination folder from the file we're trying to create and invoke `make-directory` with its second param instructing it to
create all the non-existing folders along the way. The advice example shown earlier
is doing exactly the same thing.

Place one of the above snippets in your Emacs configuration and that's it. I can never
get enough of such small productivity improvements!

That's all I have for you today. Keep hacking!
