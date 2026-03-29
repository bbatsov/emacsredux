---
layout: post
title: "Automatic Light/Dark Theme Switching"
date: 2026-03-29 11:30 +0300
tags:
- Emacs 29
- macOS
- Packages
- Themes
---

Most theme families these days ship both light and dark variants. For example,
[Tokyo Themes](https://github.com/bbatsov/emacs-tokyo-themes) has `tokyo-day`
(light) alongside `tokyo-night`, `tokyo-storm`, and `tokyo-moon` (all dark).
[Batppuccin](https://github.com/bbatsov/batppuccin-emacs) has `batppuccin-latte`
(light) and `batppuccin-mocha`, `batppuccin-macchiato`, `batppuccin-frappe`
(dark). But switching between them manually gets old fast. Here are a few ways
to automate it.

<!--more-->

## Following the OS Appearance (macOS)

Since Emacs 29, the macOS (aka `ns`/NextStep port) build can detect when the OS
switches between light and dark mode. The hook
`ns-system-appearance-change-functions` fires whenever that happens, passing the
symbol `light` or `dark` as an argument. All you need is:

``` emacs-lisp
(defun my-apply-theme (appearance)
  "Load theme based on APPEARANCE (light or dark)."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'tokyo-day t))
    ('dark (load-theme 'tokyo-night t))))

(add-hook 'ns-system-appearance-change-functions #'my-apply-theme)
```

That's it. When you flip the system appearance (or the OS does it automatically
based on time of day), Emacs follows along. The `mapc` line disables any
currently active themes first -- without it, `load-theme` stacks the new theme
on top of the old one, which can cause weird color bleed between the two.

This approach is nice because it keeps Emacs in sync with every other app on
your system. If you're on macOS and running Emacs 29+, this is probably what you
want.

**Note:** This only works with the native macOS build. If you're running Emacs
under X11 on macOS or on Linux/Windows, read on.

## Following the OS Appearance (Cross-Platform)

If you want OS appearance tracking without writing the hook yourself, or you
need it on a platform other than macOS, check out
[auto-dark](https://github.com/LionyxML/auto-dark-emacs). It detects OS-level
dark/light mode changes on macOS, Linux (via D-Bus/GNOME), Windows, and even
Android (Termux):

``` emacs-lisp
(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-themes '((batppuccin-mocha) (batppuccin-latte)))
  (auto-dark-mode))
```

The value is a list of two lists -- dark theme(s) first, light theme(s) second.
The extra nesting is there because you can stack multiple themes per mode (e.g.,
a base theme plus an overlay). For a single theme per mode, the format above is
all you need. `auto-dark` polls the system appearance every few seconds and
switches accordingly. It also provides `auto-dark-dark-mode-hook` and
`auto-dark-light-mode-hook` if you want to run extra code on each switch.

## Time-Based Switching with circadian.el

If you want theme switching based on time of day regardless of your OS, take a
look at [circadian.el](https://github.com/guidoschmidt/circadian.el). It can
switch themes at fixed times or based on your local sunrise/sunset:

``` emacs-lisp
(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '((:sunrise . batppuccin-latte)
                            (:sunset  . batppuccin-mocha)))
  (circadian-setup))
```

You can also use fixed hours if you prefer:

``` emacs-lisp
(setq circadian-themes '(("8:00"  . batppuccin-latte)
                          ("20:00" . batppuccin-mocha)))
```

For sunrise/sunset to work, set `calendar-latitude` and `calendar-longitude` in
your config. `circadian.el` uses Emacs's built-in solar calculations, so no
external services are needed.[^1]

## Rolling Your Own with run-at-time

If you don't want an extra dependency, you can do something basic with
`run-at-time`:

``` emacs-lisp
(defun my-set-theme-for-time ()
  "Switch theme based on current hour."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (mapc #'disable-theme custom-enabled-themes)
    (if (<= 8 hour 19)
        (load-theme 'tokyo-day t)
      (load-theme 'tokyo-night t))))

;; Run now and repeat every hour
(run-at-time t 3600 #'my-set-theme-for-time)
```

It's crude compared to `circadian.el`, but it works and you can tweak the
schedule however you like.

## Which One Should You Pick?

- **macOS and want zero dependencies?** The `ns-system-appearance-change-functions`
  hook is all you need.
- **Want OS tracking on Linux/Windows too?** `auto-dark` has you covered.
- **Prefer time-based switching?** `circadian.el` is the polished option;
  the DIY `run-at-time` approach works if you want to keep things minimal.

What about me? Well, I'm on macOS these days and I do enable the auto-switch
between light/dark mode there. So, normally I'd pick the first option, but
there's a small catch - I really dislike light themes for programming and I'm
using only dark variants day and night, so I don't really need theme
auto-switching in Emacs.

**Note:** One thing to keep in mind: if you're using any of these, remove any static
`load-theme` call from your init file -- let the auto-switching mechanism handle
theme loading, otherwise the two will fight on startup.

As usual, there's no shortage of ways to solve this in Emacs. Are you doing
auto-switching of themes yourself? Which is your favorite approach?

That's all I have for you today. Keep hacking!

[^1]: Am I the only one impressed by the fact that the `calendar` package can calculate things like sunrise and sunset?
