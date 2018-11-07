---
layout: post
title: "Search Youtube"
date: 2013-08-26 17:59
comments: true
tags:
- Utilities
---

Some time ago I showed you how to do
[Google queries from Emacs]({% post_url 2013-03-28-google %}). The
approach used in that articles is pretty generic and can be used for
the creation of various similar commands. Let's create a command that
searches in YouTube:

``` elisp
(defun youtube ()
  "Search YouTube with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))

```

This command will display the query results in your default browser.

I'd suggest binding the command to `C-c y` if you plan to use it regularly.

``` elisp
(global-set-key (kbd "C-c y") 'youtube)
```

`youtube` is available in
[Prelude](https://github.com/bbatsov/prelude)(but with a `prelude-`
prefix).
