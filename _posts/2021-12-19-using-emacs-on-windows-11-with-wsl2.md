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
$ git clone git://git.sv.gnu.org/emacs.git
$ sudo apt install build-essential libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo
$ cd emacs
$ ./autogen.sh
$ ./configure --with-pgtk
$ make -j8
$ sudo make install
```

The instructions above are for Ubuntu 20.04, but the steps are quite similar for any Linux distro you might be using with (or without) WSL.[^1]

Now you can type `emacs` (or `emacs-29.0.50`) in the WSL Ubuntu terminal and Emacs will start in GUI mode on Windows. It's as simple as this! If Emacs is properly packaged it will even appear in the Windows start menu, alongside any other Linux GUI apps you've installed. Epic!

![emacs_with_pgtk.png](/assets/images/emacs_with_pgtk.png)

For some reason with the default `emacs.desktop` (installed by `make install` in
`/usr/local/share/applications/`) Emacs's icon got replaced with a generic Linux
icon in Windows, but I have been unable to figure out what exactly went
wrong. I'll update the article when I figure this out.

![emacs_windows_launcher.png](/assets/images/emacs_windows_launcher.png)

Emacs 29 also ships with an improved global minor mode for scrolling with a
mouse or a touchpad, that you might want to enable as well:

``` elisp
(pixel-scroll-precision-mode)
```

Here's the description of `pixel-scroll-precision-mode` from Emacs's NEWS:

> When enabled, and if your mouse supports it, you can scroll the
> display up or down at pixel resolution, according to what your mouse
> wheel reports.  Unlike 'pixel-scroll-mode', this mode scrolls the
> display pixel-by-pixel, as opposed to only animating line-by-line
> scrolls.

In my experience this resulted in much smoother scrolling. It's not very clear to me what's the difference with the older `pixel-scroll-mode`, but the new one definitely worked better.

I did encounter one minor issue and it's related to the Windows clipboard - I can't
copy anything from Emacs to it. I believe [the issue](https://github.com/microsoft/wslg/issues/15) is well-knows and it's not Emacs specific. I've noticed that only
yanking doesn't work, if I kill some text it will actually appear in Windows's clipboard. It's also easy to come up with a workaround that shells out to `clip.exe`:

``` elisp
(defun copy-selected-text (start end)
  (interactive "r")
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties start end)))
            (shell-command (concat "echo '" text "' | clip.exe")))))
```

My solution shells out to `clip.exe` from WSL and it works reliably. The only problem with it is that you'll notice for a second the UI of `clip.exe` every time you use this command. I also didn't spend any time sanitizing the input - some texts might break the shell command (e.g. something with single quotes in it).

You might also encounter some warnings on Emacs startup that some files from the cursor theme cannot be loaded.
Basically, the problem is that most likely no GNOME theme is currently selected (simply because you don't use GNOME directly).
You can fix this by installing `gnome-tweaks`:

``` shellsession
$ apt install gnome-tweaks
```

Run the tool, select some theme from it and the warnings will go away. In general it's an useful tool to customize the visuals of GTK apps that you're running in WSL2.

I'm writing this article in Emacs 29 running on Windows 11 + WSL and it's gorgeous - gone are the blurry fonts and the need to use a 3rd party X server as a stop-gap measure. It also seems that Emacs is a bit snappier, but this might
be just my wishful thinking. One more thing - the new setup solves the annoying "X connection closed" issue that plagued some Windows X servers (e.g. X410). Before I had to restart my Emacs session almost every time my computer went to sleep and now everything works as expected. I guess it's safe to say this was my biggest motivation to switch to Windows 11 and `wslg` as soon as possible.[^2]

At this point the combination of Windows 11, WSL and Emacs is extremely powerful and enjoyable. The future seems bright!

[^1]: I've written more detailed instructions [here](https://batsov.com/articles/2021/12/19/building-emacs-from-source-with-pgtk/).
[^2]: I think that `wslg` will eventually land in Windows 10 as well.
