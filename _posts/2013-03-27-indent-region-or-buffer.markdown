---
layout: post
title: "Indent Region or Ruffer"
date: 2013-03-27 17:35
comments: true
tags:
- Editing
---

Everyone who's used Emacs for more than 10 minutes knows that you can
indent the selected region in a buffer with `C-M-\` (bound to the
command `indent-region`). While this is pretty useful I find that it's
more useful to have a command that indents the current region if
present and the entire buffer otherwise.  Therefore I've devised the
simple commands `er-indent-buffer` and `er-indent-region-or-buffer`.

``` elisp
(defun er-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun er-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (er-indent-buffer)
        (message "Indented buffer.")))))
```

Now that you have `er-indent-region-or-buffer` you don't actually need
`indent-region` or `er-indent-buffer` that much (if at all). That's why I
typically bind `er-indent-region-or-buffer` to `C-M-\`.

``` elisp
(global-set-key (kbd "C-M-\\") #'er-indent-region-or-buffer)
```
