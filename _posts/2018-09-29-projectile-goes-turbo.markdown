---
layout: post
title: "Projectile goes Turbo"
date: 2018-09-29 20:14
comments: true
tags:
- Projectile
---

For a while one of the biggest complaints people had about
[Projectile](https://github.com/bbatsov/projectile) was that the
`alien` indexing wasn't fast enough (especially on big projects). The
reason for the (relatively) bad performance was pretty simple - even
though Projectile would normally obtain the list of project files
pretty fast (e.g. by using `git ls-files`) it always did some
post-processing of the results (e.g. filtering, sorting, etc), which
is a very slow operation in Elisp on a big dataset.

Today I've added [a new indexing
method](https://github.com/bbatsov/projectile/commit/e3007ae0324fb6679a6b3dac5c63191ce907115e)
that simply dispenses with all of the post-processing and gives you
the raw power you always craved for. It's called `turbo-alien` (yeah,
yeah - naming is hard!) and it's going to be the default indexing
method going forward (starting with Projectile 1.1 which should be released pretty soon).

You can read a bit more about it in Projectile's
[manual](https://www.projectile.mx/en/latest/configuration/#project-indexing-method).

If you find yourself missing Projectile's old behaviour just add the following to your config:

``` elisp
(setq projectile-indexing-method 'alien)
```

The old tried and true `alien` method is still around, it's just no longer the default.

**P.S.** I encourage all of you to help out with some of the [open tickets](https://github.com/bbatsov/projectile/issues)
marked with "Help Wanted" or "Good First Issue" here. I'm trying to
clean-up shop after a long period of stagnation and I can certainly
use some help! :smile:

**P.P.S.** The recent 1.0 release was just a precursor to some bigger changes I
had planned to do for quite a while. Stay tuned for more updates!

**Update** Shortly after writing this post I've reconsidered the
`turbo-alien` naming and I opted to rename the old `alien` method to
`hybrid` (as it was truly a hybrid of `native` and `alien` indexing),
and to change the name of `turbo-alien` to simply `alien`. Naming is
hard! :smile:
