---
layout: post
title: Installing Emacs's Elisp Sources on Debian/Ubuntu
date: 2021-11-01 09:23 +0200
tags:
- Installation
- Debian
- Ubuntu
---

If you've installed Emacs on Debian/Ubuntu via `apt` you might have noticed that commands for
navigation to the definition of built-in Elisp functions
(e.g. `xref-find-definitions`, commonly bound to `M-.`) won't work.  That's
because for efficiency reasons (smaller payload for most users) the Debian
package maintainers decided not to ship the raw Elisp sources in the main Emacs
package. This makes perfect sense, as I highly doubt that the majority of
Emacs users often consult the implementation of functions like `shell-command` and `find-file`.
I, however, happen to be one of the few people who actually does this.

The solution for this problem is very simple - just install the package
`emacs-el` and you're good to go:

``` shellsession
$ sudo apt install emacs-el
```

Note, that depending on where did you install Emacs from, the name of the package might be different. In my case I had installed Emacs 27
from [this PPA](https://launchpad.net/~kelleyk/+archive/ubuntu/emacs) and I had to install `emacs27-el`.

``` shellsession
$ sudo apt install emacs27-el
```

That's all I have for you today. Funny enough, I've been using Emacs on Ubuntu for over a year now, but only today I bothered to
check how to fetch the missing sources. If laziness is truly a programmer virtue, it's safe to say that I've mastered this one.
