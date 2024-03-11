---
layout: post
title: Tracking World Time with Emacs
date: 2024-03-11 11:38 +0200
tags:
- Utilities
---

In today's highly connected world it's often useful to keep track of time in several
time zones. I work in a company with employees all over the world, so I probably keep track
of more time zones than most people.

So, what are the best ways to do this? I know what you're thinking - let's just
buy an Omega Aqua Terra Worldtimer mechanical watch for $10,000 and be done with
it![^1] While this will definitely get the job done and improve the looks of
your wrist immensely, there's a cheaper and more practical option for you -
Emacs. Did you know that Emacs has a command named `world-clock` that does
exactly what we want?[^2] If you invoke it you'll see something like this:

```
Seattle   Monday 11 March 02:45 PDT
New York  Monday 11 March 05:45 EDT
London    Monday 11 March 09:45 GMT
Paris     Monday 11 March 10:45 CET
Bangalore Monday 11 March 15:15 IST
Tokyo     Monday 11 March 18:45 JST
```

Hmm, looks OK but the greatest city in the world (Sofia, Bulgaria) is missing from
the list... That's totally unacceptable! We can fix this by tweaking the
variable `world-clock-list`:

``` emacs-lisp
(setq world-clock-list
      '(("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Europe/Sofia" "Sofia")
        ("Asia/Calcutta" "Bangalore")
        ("Asia/Tokyo" "Tokyo")))
```

Let's try `M-x world-clock` again now:

``` emacs-lisp
Seattle      Monday 11 March 02:51 PDT
New York     Monday 11 March 05:51 EDT
London       Monday 11 March 09:51 GMT
Paris        Monday 11 March 10:51 CET
Sofia        Monday 11 March 11:51 EET
Bangalore    Monday 11 March 15:21 IST
Tokyo        Monday 11 March 18:51 JST
```

Much better!

By the way, you don't really have to edit `world-clock-list`, as by default it's configured to
mirror the value of `zoneinfo-style-world-list`. The choice is yours.

That's all I have for you today. I hope you learned something useful. Keep hacking!

[^1]: Mechanical watches are another passion of mine.
[^2]: It was named `display-time-world` before Emacs 28.1. The command was originally introduced in Emacs 23.1.
