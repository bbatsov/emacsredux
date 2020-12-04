---
layout: post
title: Maximize the Emacs Frame on Startup
date: 2020-12-04 09:02 +0200
tags:
- UI
---

I don't know about you, but I always use Emacs maximized.[^1] It's my
most important tool[^2] and I want to dedicate to it as much screen
real estate as possible. As Emacs doesn't start in maximized mode by
default you have to do one of the following:

* Maximize it manually (e.g. by clicking the maximize icon)
* Figure out how to maximize it automatically

I guess by now it should be clear we'll be going for the second option.
As usual with Emacs there are multiple ways to achieve what we want. If
you're the type of person who starts Emacs from the command-line you might try
the `-mm` option:

    $ emacs -mm

Furthermore, you can just alias `emacs` to `emacs -mm`. Put something like this in your shell
configuration:

    alias emacs="emacs -mm"

**Tip:** There's also `emacs -fs` that will start Emacs in full-screen mode.

Another simple option would be to add something like this to your Emacs config:

``` emacs-lisp
;; the t parameter apends to the hook, instead of prepending
;; this means it'd be run after other hooks that might fiddle
;; with the frame size
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
```

Here's the description of `windows-setup-hook`:

> Normal hook run after loading init files and handling the command line.
> This is very similar to ‘emacs-startup-hook’.  The only difference
> is that this hook runs after frame parameters have been set up in
> response to any settings from your init file.  Unless this matters
> to you, use ‘emacs-startup-hook’ instead.


By the way, `toggle-frame-maximized` is an interactive Emacs command that you can use
with `M-x`. There's also a similar `toggle-frame-fullscreen` command that does exactly
what you'd expect it to do.

Finally, the most granular and slightly more involved solution would
be to leverage a couple of frame setup options - namely
`default-frame-alist` and `initial-frame-alist`. The two accept
exactly the same configuration values, but differ in their scope:

* `default-frame-alist` is applied to every Emacs frame that you create
* `initial-frame-alist` is applied only to the initial (startup) Emacs frame

I prefer the use of `initial-frame-alist`, as I rarely work with multiple frames anyways:

``` emacs-lisp
;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
```

For the completely list of frame size options, see [this
section](https://www.gnu.org/software/emacs/manual/html_node/elisp/Size-Parameters.html#Size-Parameters)
of the Emacs manual for details. The short version is that you can set
the `fullscreen`[^3] parameter to one of the following:

* `fullwidth` (make the frame as wide as possible, don't touch the vertical)
* `fullheight` (make the frame as tall as possible, don't touch the horizontal)
* `fullboth` (set height and width to to size of the screen)
* `maximized` (self-explanatory)

The difference between `fullboth` and `maximized` is that you can resize the former with the mouse, while with the latter you cannot.

That's all I have for you today. I hope you learned something useful. Meta-X forever!

[^1]: From time to time I even use it in full-screen mode, depending on how deep in the zone I am.
[^2]: Not to mention it's a pretty decent window manager as well.
[^3]: I find the name very confusing, but it is how it is.
