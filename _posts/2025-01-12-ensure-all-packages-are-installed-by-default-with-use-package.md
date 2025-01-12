---
layout: post
title: Ensure all packages are installed by default with use-package
date: 2025-01-12 18:12 +0200
tags:
- use-package
- package.el
---

I'm quite fond of `use-package` and I've organized my [personal Emacs
setup](https://github.com/bbatsov/emacs.d) around it for a while now. One thing
that I don't like very much is that by default almost I have to add
`:ensure t` to almost every `use-package` block, as I want all external packages to be
installed if they are not present.  That's quite handy when I'm setting up Emacs
on a new computer.[^1] Think something like:

```emacs-lisp
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))
```

Not a big deal for a few packages, but kind of annoying if you have 50+ packages in your `init.el`. There's a
pretty simple solution to this problem, though. Just add the following bit of configuration to your Emacs setup:

```emacs-lisp
(setq use-package-always-ensure t)
```

Now instead of specifying `:ensure t` you can specify `:ensure nil` for the packages you **don't** want to
install automatically. Note that for built-packages (e.g. `dired`) it doesn't really matter if a package
is using `:ensure t` or `:ensure nil`.[^2]

Which approach do you prefer? Are you the type of person who ensures every package is installed when absent or not? Why do you prefer one approach over the other?

[^1]: I know that a lot of people object to this approach, as you're not sure
    what versions of the packages you'd get as `package.el` is a bit primitive
    compared to something like Ruby's Bundler or Node's `npm`, but in practice
    I've rarely had issues with my approach and it has saved me a great deal of
    time.

[^2]: `package-installed-p` will return `t` for those.
