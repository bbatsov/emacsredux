---
layout: post
title: "A peek at Emacs 24.4"
date: 2013-12-31 11:07
comments: true
tags:
- misc
---

Emacs 24.4 is now feature frozen and its release is (probably) just a
few months away at this point. This makes it a good time to start
talking about some of the exciting and not so exciting changes that
will be introduced in it.

Emacs 24.4 is a special for me, since it's the first Emacs release to
which I contributed code. On a related note - I've tracked its development
much closer than I used to in previous years and I feel I have lots of interesting
things to share with you.

In a series of smallish posts I'll highlight some of the many new features that are
coming in 24.4. Keep in mind that since we're talking about unreleased code, there's
the chance of changes in the final version of 24.4.

By the way, if you're looking for a brief overview of what to expect
in Emacs 24.4 you might have a look at
[Mickey Petersen's article on the subject](http://www.masteringemacs.org/articles/2013/12/29/whats-new-in-emacs-24-4/).

The first article in the series will be arriving momentarily!

## Articles in the Series

<ul>
{% for post in site.posts reversed %}
{% if post.categories contains 'Emacs24.4' %}
<li><a href="{{ post.url }}">{{ post.title }}</a></li>
{% endif %}  <!-- categories if -->
{% endfor %} <!-- posts for -->
</ul>

**P.S.** Emacs 24.4 is fairly stable at this point, so can start using
it today. I've been using it exclusively for about half an year now
and I've rarely come across any issues whatsoever.  If you're a Prelude
user - it is 24.4 compatible.
