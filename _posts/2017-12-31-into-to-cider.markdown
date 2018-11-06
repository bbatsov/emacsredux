---
layout: post
title: "Into to CIDER"
date: 2017-12-31 10:57
comments: true
tags:
- cider
- clojure
- tools
---

[CIDER](https://github.com/clojure-emacs/cider) is a popular Clojure
programming environment for Emacs.

In a nutshell - CIDER extends Emacs with support for interactive
programming in Clojure. The features are centered around `cider-mode`,
an Emacs minor-mode that complements `clojure-mode`. While `clojure-mode`
supports editing Clojure source files, `cider-mode` adds support for
interacting with a running Clojure process for compilation, debugging,
definition and documentation lookup, running tests and so on.

You can safely think of CIDER as SLIME (a legendary Common Lisp programming
environment) for Clojure - after all SLIME was the principle
inspiration for CIDER to begin with. If you're interested in some
historical background you can check out my talk on the subject [The
Evolution of the Emacs tooling for
Clojure](https://www.youtube.com/watch?v=4X-1fJm25Ww&list=PLZdCLR02grLoc322bYirANEso3mmzvCiI&index=6).

Many people who are new to Lisps (and Emacs) really struggle with the concept of
"interactive programming" and are often asking what's the easiest (and
fastest) way to "grok" (understand) it.

While CIDER has an extensive [manual](https://cider.readthedocs.io/) and a section
on [interactive programming](https://cider.readthedocs.io/en/latest/interactive_programming/) there,
it seems for most people that's not enough to get a clear understanding
of interactive programming fundamentals and appreciate its advantages.

I always felt what CIDER needed were more video tutorials on the
subject, but for one reason or another I never found the time to
produce any. In the past this [amazing intro to
SLIME](https://www.youtube.com/watch?v=_B_4vhsmRRI) really changed my
perception of SLIME and got me from 0 to 80 in like one hour. I wanted
to do the same for CIDER users! And I accidentally did this in a way
last year - at a FP conference I was attending to present CIDER, one
of the speakers dropped out, and I was invited to fill in for them
with a hands-on session on CIDER. It was officially named [Deep Dive
into CIDER](https://www.youtube.com/watch?v=aYA4AAjLfT0), but probably
"Intro to CIDER" would have been a more appropriate name, and it's
likely the best video introduction to CIDER around today. It's
certainly not my finest piece of work, and I definitely have to
revisit the idea for proper high-quality tutorials in the future, but
it's better than nothing. I hope at least some of you would find it useful!

You might also find some of the [additional CIDER
resources](https://cider.readthedocs.io/en/latest/additional_resources/)
mentioned in the manual helpful.

Enjoy!
