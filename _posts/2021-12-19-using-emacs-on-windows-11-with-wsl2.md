---
layout: post
title: Using Emacs on Windows 11 with WSL2
date: 2021-12-19 11:09 +0200
tags:
- Windows
- WSL2
- Wayland
- pgtk
---

This article is a follow-up to an [older article]({% post_url 2020-09-23-using-emacs-on-windows-with-wsl2 %}) I wrote about running Emacs with WSL2, using an X server for Windows 10. The instructions there are still valid for Windows 11, but now we have a second simpler way for running Linux GUI apps.

Windows 11 features [built-in support for running Linux GUI applications](https://docs.microsoft.com/en-us/windows/wsl/tutorials/gui-apps).
That's great, but it requires a bit of extra work for Emacs users who have HiDPI displays,
as Windows 11 uses Wayland/Weston and [Emacs isn't a proper GTK app](https://emacshorrors.com/posts/psa-emacs-is-not-a-proper-gtk-application.html) (read this as - you'll get blurry fonts). That's not
an issue with real GTK apps like GEdit.

Yesterday, however, the pure GTK (a.k.a. `pgtk`) feature branch was [finally merged](https://www.reddit.com/r/emacs/comments/rj8k32/the_pgtk_pure_gtk_branch_was_merged/) in Emacs's master (which will become Emacs 29 in a couple of years). I'm not aware
of any pre-built Ubuntu packages that enable `pgtk`, but it's trivial to build Emacs 29 locally:

``` shellsession
$ git clone git://git.savannah.gnu.org/git/emacs.git
$ sudo apt install build-essential libgtk-3-dev libgnutls28-dev texinfo
$ cd emacs
$ ./autogen.sh
$ ./configure --with-pgtk
$ make -j8
$ sudo make install
```

You might also have to install some extra libraries, if the above commands don't work:

``` shellsession
$ sudo apt install libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev
```

I had many libraries installed prior to building Emacs, so I'm not sure what exactly was a hard build dependency. At any rate - the `configure` command will give
you clear indications if something's missing.

The instructions above are for Ubuntu 20.04, but the steps are quite similar for any Linux distro you might be using with (or without) WSL.

Now you can type `emacs` (or `emacs-29.0.50`) in the WSL Ubuntu terminal and Emacs will start in GUI mode on Windows. It's as simple as this! If Emacs is properly packaged it will even appear in the Windows start menu, alongside any other Linux GUI apps you've installed. Epic!

I'm writing this article in Emacs 29 running on Windows 11 + WSL and it's gorgeous - gone are the blurry fonts and the need to use a 3rd party X server as a stop-gap measure. It also seems that Emacs is a bit snappier, but this might
be just my wishful thinking. I think that `wslg` will eventually land in Windows 10 as well. The future seems bright!
