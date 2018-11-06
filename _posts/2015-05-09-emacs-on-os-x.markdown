---
layout: post
title: "Emacs on OS X"
date: 2015-05-09 13:31
comments: true
tags:
- osx
---

## Prelude

In this article I'll share with you a few tips and tricks about
running Emacs under the Max OS X operating system. This article will
focus on the vanilla GNU Emacs, but if you want a more native OS X experience you
might have a look at the
[enhanced Emacs Mac port](https://github.com/railwaycat/homebrew-emacsmacport/releases).

## Installation

While Emacs is available for installation from
[various sources](http://wikemacs.org/wiki/Installing_Emacs_on_OS_X)
I recommend you to use the
[Emacs for Mac OS X binary distribution](http://emacsformacosx.com/).

I always run the latest development version and I use homebrew to install it:

``` bash
$ brew install emacs --HEAD --with-cocoa
```

Keep in mind there's an ancient Emacs 22 that ships with OS X.  You
might want to alter your `PATH`, so that the new Emacs is picked up in
a shell.

Alternatively you can just create an alias in your shell and when you
invoke `emacs` it will run the newly installed version:

``` bash
$ alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
```

If you installed via Homebrew that path might look like this:

``` bash
$ alias emacs="/usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs -nw"
```

To make it permanent, if using bash, add that line to
`~/.bash_profile`. zsh users will want to update `~/.zshrc` instead.

In case you're wondering - `nw` tells Emacs to start in "terminal"
mode (instead of in GUI mode).

## Keybindings

I heartily recommend you to remap your *Caps Lock* key to *Control*. This
can be easily done via *Preferences -> Keyboard -> Modifier Keys*. If
you're using a laptop keyboard or the bluetooth keyboard you
might want to remap your right `Option` key to `Control` as
well. No one can use effectively Emacs without a right Control
key. Remapping it is a bit more involved and requires the use of the
third-party utility
[Karabiner](http://pqrs.org/macosx/karabiner/).

If you're adventurous you might even try [a crazier idea](http://emacsredux.com/blog/2013/11/12/a-crazy-productivity-boost-remap-return-to-control/).

## Setting the PATH variable

Long story short - if you're running Emacs from Spotlight (or any
other launcher for that matter) your `PATH` and `exec-path` variables
won't be same as the ones in your shell (and that's every nasty since
you want be able to run some external programs from Emacs). The best
way to handle this would be installing the package
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
by Steve Purcell.

## Flyspell

For flyspell to work correctly you'll need to install aspell plus a few dictionaries.

``` bash
$ brew install aspell --lang=en
```

## Proced

The mighty
[proced](http://emacsredux.com/blog/2013/05/02/manage-processes-with-proced/)
doesn't work on OS X.  You can use
[vkill](http://www.splode.com/~friedman/software/emacs-lisp/src/vkill.el)
as a replacement. It's kind of basic, but it mostly works.

## Dired

OS X ships with BSD's `ls` command which doesn't have all the features
of GNU `ls` (used internally by `dired`). Fortunately, this is easily solvable:

``` bash
$ brew install coreutils
```

To avoid conflicts the GNU utils are prefixed with `g`, so `ls` becomes `gls`.

``` elisp
(setq insert-directory-program (executable-find "gls"))
```

## Dash

[Dash](https://kapeli.com/dash) is the ultimately API documentation
browser. I can't live without it!

If you're using it as well, you might want to install
[dash-at-point](https://github.com/stanaka/dash-at-point).

## More goodies

If you want to spare yourself part of the headache of configuring
Emacs on OSX and get a lot of extra firepower you might want to install
[Emacs Prelude](https://github.com/bbatsov/prelude) - an enhanced
Emacs 24.x configuration (developed by yours truly) that should make
your experience with Emacs both more pleasant and more powerful.

**P.S.** I'd like to hear your tips & suggestions about making the
Emacs experience on OS X nicer and more powerful!
