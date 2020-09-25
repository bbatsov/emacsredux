---
layout: post
title: Using Emacs on Windows with WSL2
date: 2020-09-23 10:24 +0300
tags:
- Windows
- WSL2
---

I don't know about you, but I've never been able to work productively
on Windows in the past. Probably because my work requires a lot of
Unix tools and libraries, probably because of the absence of certain
Unix utilities Emacs wasn't as useful on Windows (e.g. `ls` powers `dired`, many
Emacs commands shell out to tools like `grep`, `ag`, `git`, etc).

In recent years, however, Microsoft has been trying to make amends
with the broader developer community in the form of the Windows
Subsystem for Linux (WSL). WSL is a fancy way of calling their native
VM-like functionality that allows you to run easily Linux within
Windows. Microsoft also created a powerful terminal application called
Windows Terminal (naming is hard!) that is actually capable of running
Emacs in text mode (and it does so quite well).

One practical problem with WSL,
however, is that you can't really leverage any of the Linux tools from
native Windows apps (unless you're using VS Code, that has some
special support for this). If you have a native Windows Emacs installed the
only way it can interact with the Linux VM is via TRAMP or some similar tool.

Turns out, however, you don't really need to do this, as it's quite
simple to run a GUI Emacs frame from your Linux installation inside
Windows. All you need to make this happen is an X server. There are
several options around, but I went with [X410](https://x410.dev) as it
had the simplest setup and the broadest feature set. Yeah, yeah - it's
proprietary, it's paid, but it gets the job done really well. As far
as I'm concerned those are 10 EUR very well spent.

So, time for the step by step guide:

* Setup WSL2 as documented [here](https://docs.microsoft.com/en-us/windows/wsl/install-win10) (don't forget to enable Virtualization support from your CPU from your BIOS). This guide also covers installing Windows Terminal and some Linux distro. I'll assume you went with Ubuntu 20.04.
* Install X410 from the Microsoft Store and start it.
* Allow public access in X410's settings panel (it's in the systray,
  that's something I didn't notice at first). Technically speaking,
  you're punching a hole in your Windows firewall by doing this
  (you'll be prompted by it to approve this), so be careful on public
  networks like those at airports. My Windows computer is a desktop on
  a secure home network, so that's not really a concern for me.
* Add this to your `.bashrc`:

``` bash
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
```

* Reload your `.bashrc` using `source .bashrc`.
* Start Emacs from a WSL terminal with `emacs &`.

At this point an Emacs frame will appear on your Windows
desktop. It will have an icon in the Windows task bar and
will feel like any other Windows app. Appearances are deceiving, however,
as it actually runs inside your Ubuntu VM. I have to admit I was shocked
how easy this was and how well it worked, even though running GUI apps over
X is not officially supported by WSL (at least not yet).

A note for HiDPI display owners - HiDPI scaling didn't work properly
for me (despite setting `GDK_SCALE` to 2 in my `.bashrc`) and the text
in Emacs was tiny at first, but that was easy to address - just double
your default font-size in Emacs (assuming you went with scaling factor
of 2 (200%) in Windows). I went for the following addition to my Emacs
config:

``` emacs-lisp
;; the default font size was 14
(set-frame-font "DejaVu Sans Mono 28")
```

Perhaps there are better way to handle this, but that gets the job done.
I tried some HiDPI scaling options in X410, but the results were somewhat
blurry, so I prefer my simple approach which resulted in crisp and sharp text.
Check out [X410's documentation on HiDPI](https://x410.dev/cookbook/running-x410-on-hidpi-screens/)
for more details.

One small annoyance to keep in mind is that putting your computer to sleep
will kill your Emacs GUI frame. From what I got that's a limitation of WSL2 that
can't be circumvented for now.

Also note that `systemd` doesn't work on WSL2, so you won't be able to
use it to start Emacs in daemon mode, as discussed [here]({% post_url
2020-07-16-running-emacs-with-systemd %}). Not a big deal, by any
means, just something to keep in mind.

If you run into any problems with this setup, check out X410's [excellent
documentation](https://x410.dev/cookbook/wsl/using-x410-with-wsl2/). That's
the only resource I needed.

If you feel strongly against paying for X410 there are some other X
server options that you can consider:

* [mobaXterm](https://mobaxterm.mobatek.net/)
* [VcXsrv](https://sourceforge.net/projects/vcxsrv/)
* [Xming](http://www.straightrunning.com/XmingNotes/)

I haven't tried them, however, so I can't comment on how good they are.
If you're running Emacs using some other X server for Windows, please
share your experience in the comments.

And that's a wrap for today. I'm happy to report that Windows has
never been a better option for Emacs users, and developers in general,
than it is today. This post was authored in Emacs running on WSL2, and
there's a high chance I'll keep using Windows and WSL2 for the
foreseeable future.[^1] Keep hacking!

**P.S.** After writing this article I learned that soon you'll be
able run Linux GUI apps in WSL out-of-the-box! Check out [this
article](https://devblogs.microsoft.com/commandline/whats-new-in-the-windows-subsystem-for-linux-september-2020/#gui-apps)
for more details.

[^1]: My intention was to use Linux directly, but some unfortunate driver issues with my GPU (Radeon RX 5500 XT) forced me to switch to Windows 10 for now. Once again I was reminded it's unwise to buy recent hardware and expect it to work properly with Linux. Better luck next time!
