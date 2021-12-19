---
layout: post
title: WSL-specific Emacs Configuration
date: 2021-12-19 17:02 +0200
tags:
- WSL
- Windows
---

If you're running Emacs on Windows + WSL you might have some configuration that's specific to that particular setup.[^1]
I've discovered that the simplest way to figure out if Emacs is running in WSL is to check if the host OS is Linux
and the `WSLENV` variable is present:

``` emacs-lisp
;; WSL-specific setup
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))

  ;; your code goes here
  )
```

Here's my own WSL-specific config:

``` emacs-lisp
;; WSL-specific setup
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))

  ;; pgtk is only available in Emacs 29+
  ;; without it Emacs fonts don't scale properly on
  ;; HiDPI display
  (if (< emacs-major-version 29)
      (set-frame-font "Cascadia Code 28")
    (set-frame-font "Cascadia Code 14"))

  ;; Teach Emacs how to open links in your default Windows browser
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic))))
```

Basically it does 3 things:

* Sets the default font. `Cascadia Code` is a great Windows font and I want to use it in WSL as well for consistency.
* Figures out if I'm running Emacs over X (X410) or Wayland (WSLg) and adjusts the font size based on this. I have only HiDPI displays that use a scale factor of 2 and I've disable the scaling with X410, so I compensate for this with 2 times larger fonts.
* Teaches Emacs to open links in my default Windows browser (Firefox)

That's all I have for you today. Feel free to share WSL-specific snippets from your configuration in the comments. Keep hacking!

[^1]: My personal configuration has bits and pieces specific to macOS, Linux, Windows and WSL (which is mostly Linux).
