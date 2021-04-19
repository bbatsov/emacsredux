---
layout: post
title: Configuring Minibuffer Completion in Projectile
date: 2021-04-19 10:21 +0300
tags:
- Projectile
---

[Projectile](https://github.com/bbatsov/projectile), a popular project
navigation and management package, has always supported multiple minibuffer
completion frameworks (e.g. `ido`, `ivy`, `selectrum`, etc).  A [recent
change](https://github.com/bbatsov/projectile/pull/1602) of the default
completion configuration caused a bit of confusion for Projectile's users and
inspired me to write this article.

Projectile, like almost all Emacs packages, is extremely minibuffer-centric - you'd typically get a bunch of
options in the minibuffer (e.g. a list of project files) and you have to select one of them. Projectile takes
care of calculating the things to show, but it delegates the actual job of presenting the things and making it
easy to filter them and select something to some generic minibuffer selection/completion framework.[^1]
Historically, Projectile defaulted to `ido-mode` as its minibuffer completion framework for a couple of reasons:

- back in the day `ido` didn't have many alternatives
- I somewhat foolishly assumed that everyone preferred `ido` over Emacs's default minibuffer completion

At first this was hardcoded and eventually it was made configurable via the configuration variable `projectile-completion-system`.
The arrangement worked fine for a very long time, but there was always one big problem with it - the implicit assumption
that you'd figure out that Projectile has such configuration and you'd tweak it to match whatever completion framework
you're using (e.g. `ivy` or `selectrum`). Of course, it didn't have to be this way - it would have been much better
if Projectile just auto-detected what framework (if any) you're using by default and used it as well.

That's why we've added `auto` as one of the potential values for `projectile-completion-system` and made it the default in Projectile 2.3.
This, however, showed an interested problem that we didn't foresee. Many people complained after the change that the
nice minibuffer completion, they were getting for things like finding files, had disappeared, and Projectile was broken for them.
As some of you probably realize, it wasn't that Projectile was broken - what was actually happening was that it had reverted
to using Emacs's default minibuffer completion for them (the one where you have to press `TAB` to get any results with). I had always
assumed that everyone was using something like `ido`, `ivy`, `helm` or `selectrum`, but clearly this was not the case. Turned out
that a lot of people didn't configure anything at all and for them Projectile was the only package doing "fancy" minibuffer completion.
So, if you were affected by the change you can easily revert it by adding something like this to your Emacs config:

``` emacs-lisp
(setq projectile-completion-system 'ido)
```

Still, I think you'd do better if you just enabled some completion framework globally. For `ido` you can add something like this:

``` emacs-lisp
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-ubiquitous-mode 1))
```

I, however, favor `selectrum` these days (mostly because I prefer the candidates to listed vertically):

``` emacs-lisp
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))
```

Of course, there's nothing preventing you from using a different minibuffer completion framework with Projectile and the rest of Emacs, but such a setup
seems pretty weird to me. I wonder if any of my reasons are using it and why.
Projectile's completion options are documented [here](https://docs.projectile.mx/projectile/configuration.html#completion-options) in more details.

That's all I have for you today. I do hope that the takeaways from this brief articles are clear - be careful with your assumptions, and there's a
configuration option for everything in Projectile. Keep hacking!

[^1]: I'll refer to those as "completion frameworks" for short.
