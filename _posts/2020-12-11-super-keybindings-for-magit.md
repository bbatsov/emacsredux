---
layout: post
title: Super Keybindings for Magit
date: 2020-12-11 8:34 +0200
tags:
- Magit
- Keybindings
---

This short article will present one simple, but unconventional idea -
super-powered keybindings for [Magit](https://magit.vc).[^1]

I hope by now most of you know (and use) the 3 default global Magit keybindings:

* `C-x g` (`magit-status`)
* `C-x M-g` (`magit-dispatch`)
* `C-c M-g` (`magit-file-dispatch`)

Typically you'd add to them `C-c g`, as a more convenient alternative of `C-c M-g`.[^2]

So far, so good. Now you can invoke a lot of commands like with keybindings like `C-x g l l` or `C-c g b`, which
is not bad as far as Emacs keybindings go. Remembering when to use `C-x g`, `C-x M-g` and `C-c g` takes a bit
of practice, but it's not very hard.

But what if you wanted to have a lot of Magit commands under a single two-key (super convenient) prefix?
Such prefixes are tight in Emacs these days, but only if we're talking about the commonly used prefixes based on `Control` (e.g. `C-c` and `C-x`).
If you're not a traditionalist you can try something different - use a prefix based on the rarely utilized in Emacs `Super` key.[^3]
Here's one such idea using the `Super-m` prefix (the `m`, of course, stands for Magit[^4]):

``` emacs-lisp
;; essentials
(global-set-key (kbd "s-m m") 'magit-status)
(global-set-key (kbd "s-m j") 'magit-dispatch)
(global-set-key (kbd "s-m k") 'magit-file-dispatch)
;; a faster way to invoke very common commands
(global-set-key (kbd "s-m l") 'magit-log-buffer-file)
(global-set-key (kbd "s-m b") 'magit-blame)

;; or alternatively
(use-package magit
  :ensure t
  :bind (("s-m m" . magit-status)
         ("s-m j" . magit-dispatch)
         ("s-m k" . magit-file-dispatch)
         ("s-m l" . magit-log-buffer-file)
         ("s-m b" . magit-blame)))
```

Not only are all essential Magit commands under a single mnemonic (`s-m`) right now, but you can also bind some "internal" commands that you'd
normally invoke via `magit-dispatch` or `magit-file-dispatch` directly under this prefix. This makes it slightly more efficient to invoke
frequently used commands.

The keys `m`, `j` and `k` are clustered together on a typical QWERTY keyboard which makes it super easy to press them in sequence. Of course,
you can find many other comfortable options.

One thing that you should keep in mind is that your mileage will vary based on your operating system/desktop environment. Windows, for instance,
uses `Win + letter` keybindings a lot, so `s-m` is not an option there (it minimizes the current window). Frankly, I'm not sure if any `Win + letter`
keybindings are available there at all.[^5] On macOS and Linux, however, such keybindings work fairly well, unless you happen to be using a window manager
that's fond of them (e.g. [exwm](https://github.com/ch11ng/exwm)).

I first adopted the use of `Super` [several years ago]({% post_url 2020-12-10-essential-magit-file-commands %}), but I never promoted it enough,
apart from including a few such keybindings in Emacs Prelude.
While the examples I gave today are with Magit, you can leverage the super keybindings with any package - e.g. I've long favored the
use of `s-p` for Projectile and bindings like `s-.` for `avy`.

That's all I have for you today. Super-X forever!

[^1]: In the sense of using the Super key. Although I guess one can argue that they are also super convenient.
[^2]: [This article]({% post_url 2020-12-10-essential-magit-file-commands %}) discusses the topic of the essential Magit commands in a bit more detail.
[^3]: Typically this is the `Windows` key on PC keyboards and the `Command` key on Mac keyboards.
[^4]: I sometimes wonder how the default keybindings for Magit ended up the letter "g" instead of "m". It's either a reference to Git, or that "g" happens to be on the home row. Or both.
[^5]: In theory it should be possible to tell Emacs to intercept the `Win` key before Windows for (at least) some commands, but the [suggested solution](http://ergoemacs.org/emacs/emacs_hyper_super_keys.html) never worked for me.
