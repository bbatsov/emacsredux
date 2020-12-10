---
layout: post
title: Essential Magit File Commands
date: 2020-12-10 10:07 +0200
tags:
- Magit
---

Everyone knows [Magit](https://magit.vc/) and everyone knows it's one of [my favorite
Emacs packages]({% post_url 2020-12-08-favorite-emacs-packages %}).

One thing that probably fewer people know is that once Magit is installed,
it establishes automatically several keybindings in the global keymap[^1] that allow you to
interact with Magit from any Emacs buffer in 3 different ways:

* `C-x g` (`magit-status`) - that's the way in which most people use Magit.[^2] You get a dedicated Magit buffer where you can invoke all sorts of commands for everything that you can imagine (e.g. pulling, pushing, staging, committing, branching). Generally this seems to be the best way to work with multiple files. I'm pretty sure
all of you are familiar with this way to use Magit, as it dates back to the earliest days of the project.
* `C-x M-g` (`magit-dispatch`) - that's pretty much the same as `magit-status`, but you get the opportunity to trigger a Magit command directly from the minibuffer. One can argue that's a (slightly) more effective way to work if you know it advance what you want to do (e.g. pressing `C-x M-g l l` will display the git log).
* `C-c M-g` (`magit-file-dispatch`) - that's the way to invoke Magit commands on the current file (e.g. `blame`) and that's the hero of today's article.

I've noticed that for some reason many people don't use `magit-file-dispatch` much, which seems like a wasted opportunity as it provides the
fastest way to do common things like:

* `magit-blame` (`C-c M-g b`)
* stage the current file (`C-c M-g s`)
* commit the current file (`C-c M-g c`)
* show the git log for the current file (`C-c M-g l`)
* show the diff for the current file (`C-c M-g d`)

I hope you'll agree those are pretty handy commands. `magit-file-dispatch` offers other commands as well, but let's stick to the
essential ones today. One small improvement that you can do in terms of ergonomics is to map `magit-file-dispatch` to `C-c g` instead:

``` emacs-lisp
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
```

The reasoning for this is pretty simple - it's much easier to use `C-c g` than `C-c M-g`. Unfortunately, there's a strong Emacs tradition forbidding packages
to utilize directly such keybindings (they are reserved for user keybindings), so Magit settles for the next best thing by default. You, however, have the power
to do anything you want with _your_ keybindings.

As a side note - if you're manually configuring Magit's keybindings you can stop doing this:

``` emacs-lisp
;; commonly found in the wild
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))

;; should be just
(use-package magit
  :ensure t
  :bind ((("C-c g" . magit-file-dispatch))
```

If for some reason you don't like the default Magit keybindings, you can disable them via [`magit-define-global-keybindings`](https://magit.vc/manual/magit/Default-Bindings.html).

I have to admit that for a very long time I'd use only `magit-status`
(`C-x g`) and I'd go for the file commands from Emacs's built-in `vc`
package like `vc-annotate` (`C-x v g`, a command similar to
`magit-blame`), just because I've been using Emacs since before the
creation of Git (and respectively Magit).  While the built-in `vc`
package is nice (and VCS-agnostic), Magit is way nicer when it comes
to Git support, so I'm glad I eventually managed to shed my old habits.

Magit probably has the best documentation of any Emacs package, but the problem with having a great documentation and lots of features
is that someone has to do a lot of reading. Consider this article a cheatsheet of sorts.
That's all I have for you today. I hope you've learned something useful! Feel free to share in the comments how/when do you use Magit's
3 modes of operations.

[^1]: That's a [recent change](https://github.com/magit/magit/pull/4237) from November 2020. Before it there was `global-magit-file-mode` that was serving the same purpose.
[^2]: Based on my subjective observations.
