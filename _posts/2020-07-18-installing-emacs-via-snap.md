---
layout: post
title: Installing Emacs via Snap
date: 2020-07-18 10:34 +0300
tags:
- Linux
---

**Note:** That article is only relevant for Linux users.

For as long as I can remember I've been installing Emacs (primarily) in one of two ways:

* Via the standard package manager of the operating system I'm using (e.g. `apt` on Ubuntu or `brew` on macOS)[^1]
* From source

Typically, I'd go with the first option unless I am doing some development work
on Emacs or I want to experiment with some build options, when I'd go
with the second approach. Obviously, nothing beats the convenience of the built-in
package manager, but from time to time you'd want to use a version of Emacs
that's not available in your package manager's repositories and then you have to
get more creative (e.g. find third-party repos, some (random) pre-built packages,
or build Emacs from source).

Turns out today there's a third option (at least for Linux users) - installing Emacs via `snap`.
Simply put, `snap` is a distro-agnostic package management framework that distributes self-contained
applications (they don't have any external dependencies). Think of it as something similar to Apple's App Store.
There are other similar projects in the realm of Linux (e.g. `AppImage` and `Flatpak`), but `snap`
seems to be the most popular today, mostly because it was developed by the makers of Ubuntu, Canonical.

Emacs is available at [snapcraft.io](https://snapcraft.io/emacs) in 3 flavors:

* latest stable version
* latest release candidate
* snapshot build

Provided you've already setup `snap`, installing Emacs is trivial:[^2]

``` shellsession
# install stable version
$ sudo snap install emacs --classic

# install release candidate
$ sudo snap install emacs --beta --classic

# install snapshot version
$ sudo snap install emacs --edge --classic
```

Super simple, right?

While I still plan to use `apt` most of the time (these days I'm using Ubuntu), I have to admit that
`snap` gives you a trivial way to get access to the latest and greatest version of Emacs on all major
Linux distros. I think that's a very convenient option for anyone who's not using a rolling release
distro, or simply doesn't want to waste time on upgrading their Emacs.

That's all I have for you today! Meta-end!

[^1]: In the interest of full disclosure - I've also used pre-built binary packages for macOS from <https://emacsformacosx.com/>.
[^2]: snapcraft.io provides [setup instructions for all major Linux distros](https://snapcraft.io/docs/installing-snapd). Here are the [instructions for Fedora](https://snapcraft.io/install/emacs/fedora) as an example.
