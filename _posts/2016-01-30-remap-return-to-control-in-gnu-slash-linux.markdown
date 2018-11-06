---
layout: post
title: "Remap Return to Control in GNU/Linux"
date: 2016-01-30 11:48
comments: true
tags:
- linux
- misc
---

A long time ago I wrote about
[remapping Return to Control in OS X](http://emacsredux.com/blog/2013/11/12/a-crazy-productivity-boost-remap-return-to-control/).
This was the best productivity boost for my Emacs experience ever!

Recently I've bought a Windows ultrabook (wanted something as light as
MacBook Air, but more powerful and versatile) and I'm doing most of my
work there in a Xubuntu VM. The first thing I did while setting up Xubuntu
was to figure out how to do the aforementioned remapping.

In my original post some people suggested the tool
[xcape](https://github.com/alols/xcape), so I took a look at it.  The
tool can certainly use some documentation improvements (and pre-built
packages), but it gets the job done. After you've installed it you
just need to add the following to your login shell's init file
(e.g. `.bash_profile`) and you're in business:

```
xmodmap -e "remove Control = Control_R"
xmodmap -e "keycode 0x69 = Return"
xmodmap -e "keycode 0x24 = Control_R"
xmodmap -e "add Control = Control_R"

xcape -t 10000 -e "Control_R=Return"
```

Obviously the first time around you should source `.bash_profile`
after updating it:

```
$ . .bash_profile
```

This is definitely a lot more work than just clicking in the GUI of
the wonderful [Karabiner](https://pqrs.org/osx/karabiner/), but it yields
the desired results and that's what's important at the end of the day.

Now if only there was a way to achieve the same result in Windows...

P.S. `vim` users will love `xcape`. Its default behaviour is to
generate the `Escape` key when `Left Control` is pressed and released on
its own.
