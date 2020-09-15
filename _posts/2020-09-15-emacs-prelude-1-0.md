---
layout: post
title: Emacs Prelude 1.0
date: 2020-09-15 13:50 +0300
- Prelude
---

I've got a big news to share with you today - after (over) 9 years of
development, Emacs Prelude finally made it to version 1.0! There's nothing
really big or groundbreaking there, as Prelude has been in a pretty good place for a very
long time feature-wise, but I felt like tagging a 1.0 release, because it's 2020 and
all sort of crazy things are happening the entire year. Most of you probably
don't know this, but Prelude was one of my first open-source projects[^1],
that's why making it to 1.0 means a great deal to me. Open-source projects are a
product of love, plain and simple.

I know that Prelude has probably lost some of its significance in the era of
Spacemacs, Doom Emacs, and a myriad other great Emacs distributions with more
features, sophisticated plugin systems, bells, whistles, kitchen sinks, and all that jazz, but
I don't really care. I'm happy that we made it so far, I'm glad that Prelude was
the gateway to Emacs for many people, I'm proud of the community we've built,
I'm proud that it inspired many other distributions to go in different
directions, and I'm proud that Prelude always stood true to its core philosophy:

* simple
* easy to understand and extend
* stable
* a foundation for you to build upon, as opposed to some end-user product that you're supposed to use as-is

This means that it intentionally doesn't pack all the bells and whistles that it could.
Prelude aims to enhance the classic Emacs experience without deviating a lot from it - e.g.
it would never enable something like `evil-mode` (vim keybindings) by default and so on.

All the third-party packages that it bundles are carefully vetted and are known to be of
good quality and to have reliable maintainers. That generally means that Prelude's unlikely
to immediate adopt some shiny new package, that has established tried and true alternatives.

I know some people were pissed/disappointed that in recent years Prelude didn't
change much, but for me this was never a problem - it was actually a feature, in
the same way that stability is a feature for Clojure. Fewer changes mean fewer
breakages, fewer things to learn, fewer confused users, etc. They also mean that
I believe that in terms of functionality and experience Prelude is where I want
it to be. Believe it or not, from time to time software projects are (mostly)
done.  Yeah, there's always some room for more improvements, but after a certain
point the return on investment is simply not worth it (or even worse - it
becomes negative).

I'm always amused by how many people come to CIDER's issue tracker reporting basic
issues with packages like `sayid` and `clj-refactor` (CIDER plugins) that were automatically
installed and enabled by their distribution. Most casual CIDER users don't need those
packages and the complexity that comes with them, and power users can obviously
install and configure the packages themselves. That's not a good user experience
in my book, and it's a classic example of the power of the "less is more"
philosophy. Prelude bundles only CIDER (no extensions) and it just works.
Same with many other programming languages.

Rest assured that Prelude will continue to evolve in the future, but don't
expect anything massive to change.  Lately I've been pondering switching to
`use-package` for the internals, although I don't see a strong reason to do so,
and to leverage more of LSP here and there. Leveraging some Emacs 27 features is
also on the agenda. Better documentation would be an awesome improvement as
well!

One thing I've vowed to do going forward is to keep a better track of changes
and to tag releases more often. As a result the project finally has a
[changelog](https://github.com/bbatsov/prelude/blob/master/CHANGELOG.md) and
better contribution templates. I plan (hope) to cut a couple of "stable" releases 2-3
times a year. I also plan to find a couple of co-maintainers to ensure I can
retire quietly down the road, but I'm in no rush to do this.

I guess that was one pretty weird release announcement, but that's the release
announcement I felt like writing.  Thanks to everyone who helped and used
Prelude for almost a decade! I love you all! Keep hacking!

[^1]: Projectile was the very first one.
