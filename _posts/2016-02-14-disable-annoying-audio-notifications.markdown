---
layout: post
title: "Disable Annoying Audio Notifications"
date: 2016-02-14 12:09
comments: true
tags:
- Configuration
---

By default Emacs has some pretty annoying audio notifications for
certain events (e.g. trying to move past the end of a buffer). You've
got two options to get rid of them. Option 1 is to replace them with visual
notifications (the Emacs frame will flash):

``` elisp
(setq visible-bell t)
```

This doesn't work well on OS X and is just as annoying (if not even
more), so I'd suggest going with option 2 instead - disable those
notifications completely:

``` elisp
(setq ring-bell-function 'ignore)
```

At last - some peace and quiet!
