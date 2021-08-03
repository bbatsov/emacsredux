---
layout: post
title: NonGNU ELPA Package Repository
tags:
- ELPA
- Packages
date: 2021-08-02 10:22 +0300
---
Ever since `package.el` became the standard package manager for Emacs, there has
been an official Emacs package repository called [GNU ELPA](https://elpa.gnu.org/packages/).  As of the time
of this writing it hosts around 280 packages. A respectable number, but quite
far from the 5000 packages available in the popular community-maintained
[MELPA](https://melpa.org) repository. Not to mention that many of the packages
in GNU ELPA are bundled with Emacs (meaning they originated from people involved
with Emacs's development) - e.g. `cl-lib`, `eldoc`, `xref`, etc.

Where's the huge difference of scale between ELPA and MELPA coming from? To me,
the answer is quite obvious - the amount of work a package maintainer needs to
do in both cases.  With MELPA there are no copyright assignments (and by
associations - no contribution limitations) and you don't need to host your
package in the [ELPA git
repo](https://git.savannah.gnu.org/cgit/emacs/elpa.git). As GitHub is super
popular with developers today, few people are willing to forgo it. In practice
every package that goes into GNU ELPA will likely see few external contributions as
most people are unwilling to deal with copyright assignment hassle and Emacs's bug
tracker.

The Emacs development team tried to alleviate the situation last year, by
[proposing](https://lists.gnu.org/archive/html/emacs-devel/2020-08/msg00152.html)
a default NonGNU ELPA package repository, that eliminates the need for a
copyright assignment. One year later the repository is a reality and it's
available [here](https://elpa.nongnu.org/nongnu/). NonGPU ELPA will be enabled
by default in Emacs 28, but for the time being you'll need to enable it manually like this:

``` emacs-lisp
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
```

As you can see, however, currently the repo boasts a grand total of 5 packages.
This implies that most people are probably too attached to their GitHub tool chain (and the reach that GitHub has) and are unwilling to move their projects to the
[NonGNU ELPA git repo](https://git.savannah.gnu.org/cgit/emacs/nongnu.git/). Obviously one can also adopt an approach where the development happens
mostly on GitHub and the GitHub repo changes are applied afterwards to NonGNU ELPA, but I guess few people are willing to bother with this either.

This certainly applies for me - I wouldn't mind if it was easier for Emacs users to install my packages, but I'm extremely happy with GitHub and MELPA and I'm
not willing to create additional work for myself just to make my work available on NonGNU ELPA. OSS maintainers are already way too busy and we should value
and respect their time.[^1]

As, with GNU ELPA, there's also a devel version of the NonGNU repository, for unstable releases of the packages hosted on NonGNU ELPA. You can enable it like this,
if you ever need something from there:

``` emacs-lisp
(add-to-list 'package-archives '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))
```

In the end of the day, while I welcome the efforts of Emacs's team to lower the
bar for contributions, I think they are not considering how hard it is to
compete with the GitHub development flow that most developers are accustomed to
today. As the topic of Emacs and GitHub resurfaces constantly, I won't really
dive in it - I've said many times that I'd love to see Emacs's development
happening on GitHub/GitLab, but I'm well aware this is never going to
happen. I'll just say that I doubt NonGNU ELPA will gain any meaningful traction
and disrupt the dominance of MELPA in our community. I hope that at some point
Emacs will start enabling by default [MELPA
Stable](https://stable.melpa.org/), instead of trying to compete it with
it. It's good have some consistent political views you abide by, but a bit of pragmatism
doesn't hurt either.

Going back to the topic of NonGNU ELPA. It seems to me that going forward it doesn't
make much sense to new packages to be added to GNU ELPA, unless they are going to be bundled with Emacs. Why limit the number of potential contributors for your projects for no good reason. I expect that over the course of time NonGNU ELPA will become
the primary "standard" repository. Perhaps we'll even see some packages move between GNU ELPA and NonGNU ELPA. Time will tell. Down the road I plan to try to publish
a few on my own packages to NonGNU ELPA to assess first-hand how much of overhead does it add for package maintainers. Perhaps the process is easier than I imagine it to
be.

That's all I have for you today. Keep hacking!

[^1]: It was pointed out to me that I might be mistaken about the amount of work needed to add a package hosted on GitHub to (Non)GNU ELPA. I'll try the process myself and write an update afterwards.
