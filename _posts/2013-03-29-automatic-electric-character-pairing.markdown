---
layout: post
title: "Automatic (electric) Character Pairing"
date: 2013-03-29 12:38
comments: true
tags:
- Editing
---

When editing text (and source code in particular) we often have to
deal with characters that are usually paired (like `()`, `[]`, `{}`,
etc). By default Emacs doesn't treat such characters in any special
manner and that kind of sucks, since it's quite easy to misplace a
closing parenthesis for instance.

While there are several modes that target that particular problem, in
this post I'll showcase just one of them - the built-in (since
Emacs 24.1) global minor mode `electric-pair-mode`. Enabling it is trivial:

``` elisp
(electric-pair-mode +1)
```

At this point typing an open parenthesis automatically inserts the
corresponding closing parenthesis. (Likewise for brackets, etc.)  As
an added bonus, consider the following scenario (the bar(`|`)
represents your cursor):

``` ruby
some_fun(param1, param2|)
```

Typing a closing parenthesis a this point will not insert a second
parenthesis, but will simple move the cursor over the existing one,
keeping the expression correct.

``` ruby
some_fun(param1, param2)|
```

Simple, but effective. A little bit down the road we might take a
looks at some of the (more powerful) alternatives to
`electric-pair-mode`.

`electric-pair-mode` is enabled by default in
[Prelude](https://github.com/bbatsov/prelude).
