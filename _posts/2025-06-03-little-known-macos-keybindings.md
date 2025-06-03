---
layout: post
title: Little known macOS keybindings
date: 2025-06-03 09:48 +0300
---

Today's article is going to be a bit more weird than usual... mostly because I've
set to write about one topic, and ended up about writing something completely different
in the end... Here we go!

I'm guessing 99% of Emacs users know that the most common ways to start isearch
are with `isearch-forward` (`C-s`) and `isearch-backward` (`C-r`). That's not
the full story, though!  While working on my [recent isearch article]({%
post_url 2025-03-18-you-have-no-idea-how-powerful-isearch-is %}) I noticed that
out-of-the-box there are two other keybindings for those commands:

- `s-f` (`isearch-forward`)
- `s-F` (`isearch-backward`)

**Note:** `s` in this context means `Super`, which is usually `Win` in Windows and `Command`
in macOS.

When I saw those I was like "hmm, seems someone wanted to make Emacs a bit more
approachable to macOS users coming other editors". But here things got
interesting...

I tried to find out where those extra keybindings were defined, and
after a bit of digging I found them in the `ns-win.el` library[^1], which defines a
ton of macOS-specific keybindings:

```emacs-lisp
;; Here are some Nextstep-like bindings for command key sequences.
(define-key global-map [?\s-,] 'customize)
(define-key global-map [?\s-'] 'next-window-any-frame)
(define-key global-map [?\s-`] 'other-frame)
(define-key global-map [?\s-~] 'ns-prev-frame)
(define-key global-map [?\s--] 'center-line)
(define-key global-map [?\s-:] 'ispell)
(define-key global-map [?\s-?] 'info)
(define-key global-map [?\s-^] 'kill-some-buffers)
(define-key global-map [?\s-&] 'kill-current-buffer)
(define-key global-map [?\s-C] 'ns-popup-color-panel)
(define-key global-map [?\s-D] 'dired)
(define-key global-map [?\s-E] 'edit-abbrevs)
(define-key global-map [?\s-L] 'shell-command)
(define-key global-map [?\s-M] 'manual-entry)
(define-key global-map [?\s-S] 'ns-write-file-using-panel)
(define-key global-map [?\s-a] 'mark-whole-buffer)
(define-key global-map [?\s-c] 'ns-copy-including-secondary)
(define-key global-map [?\s-d] 'isearch-repeat-backward)
(define-key global-map [?\s-e] 'isearch-yank-kill)
(define-key global-map [?\s-f] 'isearch-forward)
(define-key esc-map [?\s-f] 'isearch-forward-regexp)
(define-key minibuffer-local-isearch-map [?\s-f]
  'isearch-forward-exit-minibuffer)
(define-key isearch-mode-map [?\s-f] 'isearch-repeat-forward)
(define-key global-map [?\s-F] 'isearch-backward)
(define-key esc-map [?\s-F] 'isearch-backward-regexp)
(define-key minibuffer-local-isearch-map [?\s-F]
  'isearch-reverse-exit-minibuffer)
(define-key isearch-mode-map [?\s-F] 'isearch-repeat-backward)
(define-key global-map [?\s-g] 'isearch-repeat-forward)
(define-key global-map [?\s-h] 'ns-do-hide-emacs)
(define-key global-map [?\s-H] 'ns-do-hide-others)
(define-key global-map [?\M-\s-h] 'ns-do-hide-others)
(define-key global-map [?\s-j] 'exchange-point-and-mark)
(define-key global-map [?\s-k] 'kill-current-buffer)
(define-key global-map [?\s-l] 'goto-line)
(define-key global-map [?\s-m] 'iconify-frame)
(define-key global-map [?\s-n] 'make-frame)
(define-key global-map [?\s-o] 'ns-open-file-using-panel)
(define-key global-map [?\s-p] 'ns-print-buffer)
(define-key global-map [?\s-q] 'save-buffers-kill-emacs)
(define-key global-map [?\s-s] 'save-buffer)
(define-key global-map [?\s-t] 'menu-set-font)
(define-key global-map [?\s-u] 'revert-buffer)
(define-key global-map [?\s-v] 'yank)
(define-key global-map [?\s-w] 'delete-frame)
(define-key global-map [?\s-x] 'kill-region)
(define-key global-map [?\s-y] 'ns-paste-secondary)
(define-key global-map [?\s-z] 'undo)
(define-key global-map [?\s-+] 'text-scale-adjust)
(define-key global-map [?\s-=] 'text-scale-adjust)
(define-key global-map [?\s--] 'text-scale-adjust)
(define-key global-map [?\s-0] 'text-scale-adjust)
(define-key global-map [?\s-|] 'shell-command-on-region)
(define-key global-map [s-kp-bar] 'shell-command-on-region)
(define-key global-map [?\C-\s- ] 'ns-do-show-character-palette)
(define-key global-map [s-right] 'move-end-of-line)
(define-key global-map [s-left] 'move-beginning-of-line)

(define-key global-map [home] 'beginning-of-buffer)
(define-key global-map [end] 'end-of-buffer)
(define-key global-map [kp-home] 'beginning-of-buffer)
(define-key global-map [kp-end] 'end-of-buffer)
(define-key global-map [kp-prior] 'scroll-down-command)
(define-key global-map [kp-next] 'scroll-up-command)

;; Allow shift-clicks to work similarly to under Nextstep.
(define-key global-map [S-mouse-1] 'mouse-save-then-kill)
(global-unset-key [S-down-mouse-1])

;; Special Nextstep-generated events are converted to function keys.  Here
;; are the bindings for them.  Note, these keys are actually declared in
;; x-setup-function-keys in common-win.
(define-key global-map [ns-power-off] 'save-buffers-kill-emacs)
(define-key global-map [ns-open-file] 'ns-find-file)
(define-key global-map [ns-open-temp-file] [ns-open-file])
(define-key global-map [ns-open-file-line] 'ns-open-file-select-line)
(define-key global-map [ns-spi-service-call] 'ns-spi-service-call)
(define-key global-map [ns-new-frame] 'make-frame)
(define-key global-map [ns-toggle-toolbar] 'ns-toggle-toolbar)
(define-key global-map [ns-show-prefs] '
```

Some of them look quite convenient (easy to press), so I might add a few
to my daily work. I'm shocked I never trying any of the standard macOS
keybindings for things like adjusting text size in Emacs. Or perhaps I tried
them and then I forgot about them... :D

Still, even though I'm a macOS users (at least for the time being), I doubt I'll end
up using many of them. The reason for this is that I learned Emacs on Linux
and I'm extremely used to the default keybindings. Between remembering all of those,
and trying to master Vim (as of late), it's hard to teach this old dog any new tricks.
That being sad, I can imagine those keybindings being useful to many other people, especially
if they haven't learned Emacs on Linux 20 years ago.

**Tip:** Do a `M-x find-library RET ns-win` to see what else the library has in store
for macOS users.

That's all I have for you today! Keep hacking!

**P.S.** After writing this article I was really amused that I've been using macOS on and
off for over 10 years and I never bothered to try whether something like `Command-s` or
`Command-z` works in Emacs! Oh, well... habits!

[^1]: Emacs stubbornly keeps refering to macOS by its ancient name NextStep in much of the code and its documentation.
