---
layout: post
title: Emacs and XDG sitting on a tree
date: 2025-01-12 18:34 +0200
tags:
- Configuration
---

Where to place my Emacs configuration? That is the question!

This fairly simple question has surprisingly many answers, as it often happens with projects
as old as Emacs:

- Historically Emacs's user config was a file called `.emacs`, placed in your home directory
- Later you could use the name `init.el` as an alias to `.emacs`
- And then there's the option that's probably most common today - placing your configuration
under the `.emacs.d` folder, where you usually have an `init.el` that potentially refers to
other Emacs Lisp libraries

But wait, there's more! Emacs 27.1 introduced `XDG_CONFIG_HOME` support, and Emacs 29.1 extended the
level of XDG support in Emacs.[^1] Most of the time the XDG config home would be `~/.config` and there would be
a subfolder there for the configuration of each application. In Emacs's case that would be `~/.config/emacs`.
I hope it's clear that the idea here is to reduce the clutter at the top-level and to make sure that each
application stores its configuration in a predictable place.

I think it's really great that Emacs is (slowly) adopting the industry standards
and I believe that over the course of time the XDG config folder will become the
preferred place to store your Emacs configuration. That's why I encourage
everyone to move in this direction and unclutter their home folder a bit.

Just keep in mind a couple of things:

- You'll have to make sure that folders like `~/.emacs.d` don't exist anymore, as they'd have precedence over the XDG config folder
- Some packages might have hardcoded the path in which they store their own configuration. In general they should be relying on `user-emacs-directory`, which will be auto-set to whatever directory Emacs discovered it's configuration in, but there will always be some packages that probably didn't do "the right thing".

I'm guessing that we're not really getting rid of `~/.emacs.d` any time soon (or ever), but I'm hoping that article like this one might speed up a bit the process. Time will tell.

So, has anyone moved their Emacs config to respect the XDG conventions already? How smooth was the process for you?

**P.S.** If you're curious to learn more about how Emacs's configuration discover process I'd suggest reading
[this chapter](https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html) of the Emacs Manual. Good stuff!

[^1]: Check out [the XDG specification](https://specifications.freedesktop.org/basedir-spec/latest/) if you're not familiar with XDG.
