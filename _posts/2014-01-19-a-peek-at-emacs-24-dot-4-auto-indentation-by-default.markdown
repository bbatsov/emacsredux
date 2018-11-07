---
layout: post
title: "A peek at Emacs 24.4: auto-indentation by default"
date: 2014-01-19 08:52
comments: true
tags:
- Emacs24.4
---

I've written in the past about
[electric-indent-mode]({% post_url 2013-03-29-automatic-electric-indentation %}),
which was added in Emacs 24.1. In Emacs 24.4 one of the most prominent
user visible changes is that it's enabled out-of-the box. That's a
huge step towards the "modernization" of Emacs and one of the bigger
changes to the defaults in recent times. Let's review briefly how the
mode works with a couple of Ruby examples (`|` signifies the cursor
position). Without `electric-indent-mode`:

``` ruby
def something|
```

After you press `Return` you'll get:

``` ruby
def something
|
```

With it:

``` ruby
def something|
```

After you press `Return` you'll get:

``` ruby
def something
  |
```

Nice, ah?

One problem with `electric-indent-mode` is that it doesn't play nice
with some (mostly third-party) modes (`yaml-mode`, `slim-mode`,
etc). I guess the situation will improve over time, but for now you
can simply disable the mode in such problematic cases:

``` elisp
(add-hook 'yaml-mode-hook (lambda () (electric-indent-local-mode -1)))
```

Note that `electric-indent-local-mode` was introduced in Emacs 24.4.

If you want to make a major mode electric-indent aware, have a look at
the documentation of `electric-indent-functions` and
`electric-indent-chars`.

**P.S.**

Dmitry Gutov recently wrote
[more on the topic](http://dgutov.github.io/blog/2014/01/20/electric-indentation-in-ruby-in-emacs-24-dot-4/)
in the context of `ruby-mode` in Emacs 24.4.
