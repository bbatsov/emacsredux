---
layout: post
title: "A Crazy Productivity Boost: Remap Return to Control"
date: 2013-11-12 17:09
comments: true
tags:
- Productivity
- macOS
---

People have always complained about the awkward positioning of
the two `Control` keys on modern keyboards.[^1] That's a fact! Effective Emacs
usage is heavily dependent on the `Control` keys (yep, both of
them). That's also a fact!

A great many Emacs users remap the infrequently used `CapsLock` key to
`Control` to alleviate partially the problem with the accessibility of
the control keys. That, while useful, is not sufficient for the
optimal typing experience, since that way you're breaking the key
symmetry on both sides of your keyboard. Also - your right pinky has
to go much further than your left one, while you're typing. Many
people seem to be using only the left `Control` and I guess they're
not particularly bothered by this, but touch typists like me are
generally quite bothered by such things.

A much better idea would be to leverage a little knows capability of
keyboard input systems and map the `Return` key to `Control` only
when it's held down (it will behave like a normal `Return` key in all
other situations). This sounds a bit crazy, but please bear with me for a while.

This radical approach has several advantages.  First and foremost -
it's much easier to hit `Return` with your right pinky than it is to
hit the regular right `Control` (especially if you're using a US
layout keyboard - these have long single row `Return` keys, compared
to the short 2 row Returns found on European keyboards). Second, if
you've already remapped `CapsLock` to `Control` (like you should have
done) you're getting a pretty symmetrical mapping on the opposite side
of your keyboard. Last, but not least - it's a great option for people
using compact keyboards with no left `Control` key.

Obviously you'll need some keyboard remapping software to make this
trick work. OS X users can use
[KeyRemap4MacBook](http://pqrs.org/macosx/keyremap4macbook/) to do
that.  In its settings look up the `Change Return` section and under
it `Return to Control_R (+ When you type Return only, send
Return)`. As far as I know this remapping can be done on GNU/Linux systems with [xcape](https://github.com/alols/xcape)
(though I haven't checked that myself), but I have no idea if it's a viable option for Windows users.

All in all - remapping `Return` to `Control` should be a big
productivity win for some (the touch typists) of you and should make
your right pinky's life easier.

**Update:** In 2017 I've published [a small update]({% post_url 2017-12-31-a-crazy-productivity-boost-remapping-return-to-control-2017-edition %}) on this topic.

[^1]: If you are lucky enough to have two of them. Damn you Apple laptops!
