---
layout: post
title: State of Emacs Survey
date: 2020-10-22 09:02 +0300
tags:
- Meta
---

Recently I learned of the first-ever [State of Emacs survey](https://emacssurvey.org/)
and I wanted to share a few thought about it with my readers. Okay, the survey is officially
named just "Emacs Survey", but it clearly draws a lot of inspiration from the numerous
"State of Technology X" surveys that have become popular in recent years. That's why
I took the liberty to re-frame it in a way that will probably be familiar to more people.

Those "State of Something" surveys are typically ran by the teams
maintaining some project to assess the health of its ecosystem, the
needs of their users, the problems that need to be solved, the tools
that people are using and so on. A good example would be the [State of
Clojure](https://clojure.org/news/2020/02/20/state-of-clojure-2020),
that I participate in every year. There are similar surveys for many
programming languages and even for some tools - e.g. I ran in the past
a [State of
CIDER](https://metaredux.com/posts/2019/11/02/state-of-cider.html)
survey.[^1] Ideally, the data collected from the surveys drives the
future development to some extent, and results in a better experience
for everyone involved. I can speak from experience that the "State of Clojure" and
"State of CIDER" surveys definitely influenced the direction of the projects.

The "State of Emacs" survey is a somewhat different, though.  One
thing that bothers me a bit about the Emacs survey is that its site
doesn't list any clear purpose/goals for the survey. That definitely
seems pretty weird to me. I saw the survey discussed on the Emacs
mailing list and on
[Reddit](https://www.reddit.com/r/emacs/comments/je3eht/emacs_user_survey_2020_is_open/),
but sticking a paragraph of rationale on the survey site would have certainly helped. The
survey is also not official in the sense, that it's not initiated by
Emacs's development team, and judging by the hostility the topic
generated on the `emacs-devel` mailing list it's not like they were particularly
excited about it.[^2] I was especially upset about the hostility towards
the amazing MELPA project on the mailing list. Implying that it'd be easy to replace MELPA with some
core Emacs repository is like a bad joke, given how the deficiencies of GNU ELPA
were the reason for the rise of MELPA in the first place. Anyways, I'm digressing.

Despite of all my concerns, I hope that the survey be a success in the
sense that it will collect enough useful data points to influence the
future development roadmap of Emacs and align better the vision of
Emacs's maintainers with the needs and the usage patterns of their
users.  I've already filled out the survey myself and I liked the
questions in it, although, I would have definitely structured it
somewhat differently. I see some constant trend to compare stuff in
Emacs with external packages (e.g. Magit vs `vc`, Flycheck vs Flymake,
Projectile vs `project.el`), which I find slightly bizarre given the
trend in Emacs to move as many built-in packages as possible to GNU
ELPA, and the fact that Emacs has always been about diversity of
solutions, picking whatever works best for you and so
on. Standardization efforts seems somewhat against the spirit of
Emacs, but that's just my very personal take.[^3]

In case someone is curious about my Emacs wish list - I don't really
care about changing any defaults, package improvements or anything
like this.  I'd like to see more efforts for improving Emacs Lisp, the
standard library (efforts like `seq.el` and `map.el` were great IMO),
making it possible to built rich UIs in some sane manner (overlays are
quite limiting). I'd also love to see the bar to contributing to be lowered:

* drop the contributor agreement
* discuss ideas in an (modern) issue tracker, instead of on a mailing list
* apply less political activism and more pragmatism in the conversation around new ideas/features

I doubt anything like this is going to happen any time soon, but one can dream, right?

Finally, I'm obviously curious how my own Emacs
Prelude and my packages like Projectile, crux and so on will fare in
the survey. Are Zenburn and Solarized still the most popular color themes out there?
You tell me! :-)

The survey will be open until the end
of November. You can fill out the survey form directly
[here](https://form.jotform.com/202884894078067). Share your perspective!

[^1]: CIDER is a personal project of mine.
[^2]: That being said, way too many topics generate unwarranted hostility there.
[^3]: Some degree of standardization is not a bad thing, though.
