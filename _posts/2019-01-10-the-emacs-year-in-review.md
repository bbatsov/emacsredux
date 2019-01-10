---
layout: post
title: The Emacs Year in Review
date: 2019-01-10 10:37 +0200
tags:
- Meta
---

This post is a brief summary of the past year for Emacs from my perspective.

## Emacs 26.1

Probably the biggest news of the year was the release of [Emacs 26.1](https://www.gnu.org/software/emacs/news/NEWS.26.1).
The highlights of the release were:

* Limited form of concurrency with Lisp threads
* Support for optional display of line numbers in the buffer
* Emacs now uses double buffering to reduce flicker on the X Window System
* Flymake has been completely redesigned
* TRAMP has a new connection method for Google Drive
* New single-line horizontal scrolling mode
* A systemd user unit file is provided
* Support for 24-bit colors on capable text terminals

Frankly, for me that release was a bit underwhelming as I won't see
much improvement in my day-to-day use of Emacs.  I'm on macOS, I
don't use Flymake, I don't use TRAMP, I don't like seeing line numbers
and I don't use Emacs in terminal mode.  What a bummer, right?

I'm excited that we finally got some limited form of concurrency,
though. Probably this is going to become important in a few years, as
Emacs packages start adopting it. There are also plenty of other small
and very useful improvements in this release. Mickey (from "Mastering
Emacs") goes over the release notes
[here](https://www.masteringemacs.org/article/whats-new-in-emacs-26-1)
in greater detail. Maybe I'll do something similar in the future if I
ever find the time for it.

## My Emacs Packages

It was a super busy year at work for me, but still I got to release
new versions of most of my [Emacs
Packages](https://metaredux.com/projects).  I'm really proud of
releasing several big CIDER updates, and making it to version 1.0 (and
recently 2.0) for Projectile!

Things were quieter on the Prelude front, but I think it's pretty
good, useful and stable in its current form.  With everyone these days
trying to pile every possible feature and package in an Emacs
distribution, one has to appreciate the tenets of Prelude's
philosophy:

* simple
* easy to understand and extend
* a foundation for you to build upon, as opposed to some end-user product

Probably I should expand on this down the road as well...

Overall, I've gotten to a point where I don't have time to properly
maintain all of my projects and it seems that I'll have to focus on
fewer of them in the future and solicit help from other people for the
packages I can't find enough time for.

## Emacs Redux

I didn't write much here last year, but at least I managed to overhaul the
blog's visuals and simplify its setup. Moving from Octopress to Jekyll
really simplified things and I hope this will result in more articles down the road.

I've also started a new personal blog - [Meta
Redux](https://metaredux.com). You're more than welcome to check it
out!

## Emacs Packages

I don't recall many new Emacs packages that made the news in 2018. I
think I was most excited about
[ELSA](https://github.com/emacs-elsa/Elsa) - a brand new Emacs Lisp
Static Analyzer.  I've also noticed that many people were excited
about [LSP](https://langserver.org/) in Emacs and the older
[lsp-mode](https://github.com/emacs-lsp/lsp-mode) got some competition
in the face of [eglot](https://github.com/joaotavora/eglot).  As all
of the programming I do these days is in Emacs Lisp or Clojure, I
don't really need LSP (generally LSP makes little sense for
REPL-driven programming), but it's great that things are making
headway there.

Many of the great Emacs packages became even greater this year - e.g. Magit, company-mode, avy, ivy, etc.

I didn't pick up many new (for me) packages this year. I can think only of:

* After many years of using `ido` I migrated to [ivy](https://github.com/abo-abo/swiper#ivy) and I'm super happy with it
* I'd dropped my custom comment annotations highlighting code in favour of [hl-todo](https://github.com/tarsius/hl-todo)
* I've rediscovered [easy-kill](https://github.com/leoliu/easy-kill)
* I've discovered how awesome [AsciiDoc](https://asciidoctor.org/) is
  (so much better than Markdown for writing technical
  documentation!!!) and I've started using
  [adoc-mode](https://github.com/sensorflo/adoc-mode). Unfortunately
  it's somewhat buggy and incomplete, and it hasn't seen a commit in 3
  years. It'd be great if we had a better AsciiDoc mode for Emacs!

I'll also add a shoutout here for
[Buttercup](https://github.com/jorgenschaefer/emacs-buttercup) - the
best testing library Emacs has to offer today! It's so much better and
easier to use than ERT, that I'm shocked so few people have discovered
it yet. It definitely deserves a blog post or two!

As usual the packages I relied on the most this year were my own
[Projectile](https://github.com/bbatsov/projectile) and
[crux](https://github.com/bbatsov/crux).

My color theme is forever [Zenburn](https://github.com/bbatsov/zenburn-emacs). I've used it for over a decade
and I still can't find an alternative so appealing that it would make me switch!

## MELPA

MELPA really crushed it this year and solidified its position as the only `package.el` repo that really matters.
At this point it's like `homebrew` for macOS - it has alternatives, but pretty relatively few people are using them.
I'm happy about the consolidation of the package repo scene, but I'm a bit worried that still most people are
installing snapshots instead of stable package releases.

Of course, that's on MELPA - it's on package maintainers who have adopt a more disciplined approach to releases.

## Misc

I really loved reading ["The Evolution of Emacs
Lisp"](https://www.iro.umontreal.ca/~monnier/hopl-4-emacs-lisp.pdf)
paper by Emacs's former head maintainer Stefan Monnier and Michael
Sperber. I've been using Emacs for 15 year now and I still learned a
lot of new things about the history of the language and the rationale
behind certain design decisions. It was also a bit depressing to see
how long certain features were being developed without ever making it
to a stable Emacs release, but it is what it is...

As usual, throughout the year my best source of Emacs news and updates
was [Emacs News](http://sachachua.com/blog/category/emacs-news/) by
Sacha Chua. I can't recommend it highly enough!

## Looking Forward

Frankly, there's nothing big I'm looking forward to in 2019. I'd love for Emacs to finally start
better supporting operating systems like Windows and macOS, but I know that's a pipe dream at this point.

I do hope that more of the packages bundled with Emacs today would be moved to GNU ELPA, so they can be released
separately and more frequently. Emacs's core should become smaller IMO and the big focus of the Core Team should be
improving the basic editing experience, UI and the core API libraries. And, of course, Emacs Lisp itself.
I really don't think that Emacs will ever replace Emacs Lisp with Common Lisp or Scheme (Guile), so we'd better
develop a better version of Emacs Lisp instead.

By the way, I think it might be nice if the Emacs Team started running some annual "State of Emacs" survey similar in
spirit to say to the ["State of Clojure" survey](https://clojure.org/news/2019/01/07/clojure-2019-survey).

On a personal note I hope that I'll write a few more articles here in 2019 and that I'll manage to get CIDER to the coveted
1.0 release and Projectile to the next level. I'm also planning to work a bit on `project.el`, so it'd play nicer with
Projectile and provide a better foundation for tools like Projectile.

I also have some vary vague plans to work on improving
`erlang-mode`, take a stab at creating a new `asciidoc-mode` and maybe
play a bit with Haskell-related packages (if I finally manage to
learn Haskell that is). Time will tell whether any of this is going to happen.

I'm try to be more active here, but I'm not making any promises.  The
last couple of years really drained me and as much I'd love to work on
many things it would probably be best for me not to work on anything
at all.

## Closing Thoughts

So, that was Emacs's 2018 from my perspective. What about yours?

I'd really love to hear what were the Emacs highlights of the year
from your perspective. Please, share in the comments what were you
most excited about in 2018 and what are you looking forward to in the
future.

Emacs had another great year and it's all because of you - the great community around it!
In M-x we trust!
