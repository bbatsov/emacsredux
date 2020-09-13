---
layout: post
title: Describe Package
date: 2020-09-13 16:01 +0300
tags:
- Package Management
- Utilities
---

I don't know about you, but I often need to see some information about a package - e.g.
the version I've installed, what the package dependencies are, its homepage, the latest available version, etc.
While there are numerous ways to do this in Emacs (e.g. using `find-library` or using `package-list-packages`),
I definitely believe there's one command that provides better experience than all the other options - namely `describe-package`.
So, what makes this command so great?

First of all - it has a convenient default keybinding (`C-h P`).[^1] Another
nice benefit is that the command will prompt you for the package to describe,
regardless of whether it's installed or not. This means you don't really need to
know the exact name of the package you're looking for. Last, but not least,
you'll get a nice little summary buffer from which you can also install or
delete a package. Here's how that buffer looks for my beloved CIDER package:

![describe-pkg.png](/assets/images/describe-pkg.png)

As you can see, all the important package information is there.[^2] One really
nice touch is that for installed packages you also see their latest available
version. It seems I haven't updated CIDER in month! I guess I know what I'll be
doing after publishing this article.

That's all I have for you today! Keep hacking!

[^1]: I typically bind `find-library` to `C-h l`, as I use that one quite often as well.
[^2]: Most of it is extracted from the package front-matter.
