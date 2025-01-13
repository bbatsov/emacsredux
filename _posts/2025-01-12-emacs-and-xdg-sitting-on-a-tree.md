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
level of XDG base directory specification support in Emacs.

XDG, which stands for X Desktop Group, is a set of standards and specifications
developed by freedesktop.org to promote interoperability among different desktop
environments, primarily in Unix-like operating systems. The group focuses on
ensuring that applications can work seamlessly across various desktop
environments by adhering to common guidelines.

Their most notable work to date is the XDG base directory specification.  In a
nutshell this specification outlines the environment variables that define where
user-specific data files, configuration files, and cache files should be
stored. For instance, it specifies directories like `$XDG_CONFIG_HOME` for
configuration files and `$XDG_DATA_HOME` for data files.[^1]

Most of the time the XDG config home would be `~/.config` and there would be
a subfolder there for the configuration of each application. In Emacs's case that would be `~/.config/emacs`.
I hope it's clear that the idea here is to reduce the clutter at the top-level and to make sure that each
application stores its configuration in a predictable place.

I think it's really great that Emacs is (slowly) adopting the industry standards
and I believe that over the course of time the XDG config folder will become the
preferred place to store your Emacs configuration. That's why I encourage
everyone to move in this direction and unclutter their home folder a bit.

Just keep in mind a couple of things:

- You'll have to make sure that folders like `~/.emacs.d` don't exist anymore, as they'd have precedence over the XDG config folder. The configuration resolution happens like this:

> Emacs looks for your init file using the filenames `~/.emacs.el`, `~/.emacs`, or `~/.emacs.d/init.el` in that order; you can choose to use any one of these names. (Note that only the locations directly in your home directory have a leading dot in the location’s basename.)
>
> Emacs can also look in an XDG-compatible location for init.el, the default is the directory `~/.config/emacs`. This can be overridden by setting `XDG_CONFIG_HOME` in your environment, its value replaces `~/.config` in the name of the default XDG init file. However `~/.emacs.d`, `~/.emacs`, and `~/.emacs.el` are always preferred if they exist, which means that you must delete or rename them in order to use the XDG location.
>
> Note also that if neither the XDG location nor `~/.emacs.d` exist, then Emacs will create `~/.emacs.d` (and therefore use it during subsequent invocations).
>
> Emacs will set user-emacs-directory to the directory it decides to use.

Not the best defaults IMO (especially falling back to creating `.emacs.d`), but you can't fight tradition! Or rather - fighting tradition is pretty hard...

- Some packages might have hardcoded the path in which they store their own configuration. In general they should be relying on `user-emacs-directory`, which will be auto-set to whatever directory Emacs discovered its configuration in, but there will always be some packages that probably didn't do "the right thing".

I'm guessing that we're not really getting rid of `~/.emacs.d` any time soon (or ever), but I'm hoping that article like this one might speed up a bit the process. Time will tell.

macOS users should keep in mind that unfortunately macOS doesn't set the standard XDG environment variables, as it has its own notion of where things like configuration files, application cache, etc should be stored. Still, it's fairly easy to just set the missing variables yourself (e.g. in your `.zshrc`):

```bash
export XDG_CONFIG_HOME = $HOME/.config
export XDG_DATA_HOME = $HOME/.local/share
export XDG_STATE_HOME = $HOME/.local/state
export XDG_CACHE_HOME = $HOME/.cache
```

So, has anyone moved their Emacs config to respect the XDG conventions already? How smooth was the process for you?

**P.S.** If you're curious to learn more about how Emacs's configuration discover process I'd suggest reading
[this chapter](https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html) of the Emacs Manual. Good stuff!

[^1]: Check out [the XDG base directory specification](https://specifications.freedesktop.org/basedir-spec/latest/) for more details.
