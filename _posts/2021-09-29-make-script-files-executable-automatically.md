---
layout: post
title: Make Script Files Executable Automatically
date: 2021-09-29 13:59 +0300
tags:
- Editing
---

How does the process of script creation typically go? You create a file (e.g. `script.py`),
you write some code in it, and finally you make the file executable, so you can
run it directly (e.g. by typing `./script.py`). Turns out that Emacs has one very
specific helper for making a script executable, namely the function
`executable-make-buffer-file-executable-if-script-p`.

What the function does is to check if the buffer file has a shebang (e.g. `#!/bin/ruby`) in it and then it modifies its permissions, if necessary. The function is meant to be used
in a hook, most typically `after-save-hook`:

``` emacs-lisp
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
```

I've been using this little trick for ages, and I was reminded of it today
when I came across [this Prelude issue](https://github.com/bbatsov/prelude/issues/1343). I guess in some cases making a file executable automatically might be
undesirable, but I've never ran into any issues myself.

That's all I have for you today. Feel free to share other tips for working with
scripts in Emacs in the comments. Keep hacking!
