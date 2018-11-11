---
layout: post
title: "Removing/Altering Key Bindings from Minor Mode Keymaps"
date: 2013-09-25 12:26
comments: true
tags:
- Configuration
- Keybindings
- Minor Modes
---

Often minor modes don't respect standard keybinding conventions and
use some user reserved keys (like `C-c a`). Occasionally two minor
modes would have conflicting keybindings or a minor mode would
keybindings conflicting with a major mode. I guess you can imagine
similar problems. Dealing with them is fairly straight-forward - we
have to either unbind or rebind the offending binding:

``` elisp
;; remove keybinding
(define-key minor-mode-map (kbd "C-c c") nil)

;; change keybinding
(define-key minor-mode-map (kbd "C-c c") (kbd "C-c C-c"))
```

Generally speaking you can use `define-key` to alter the keymap of a
major mode as well, but those are rarely offenders when it comes to
picking appropriate keybindings for their keymaps.

Normally you'd want to invoke the above code right after the related
minor (or major) mode is loaded:

``` elisp
(eval-after-load "minor-mode"
  '(define-key minor-mode-map (kbd "C-c c") nil))
```

Making a minor mode have different keymaps in different major modes is
tricky, but possible. Here's an example that disables some keybindings
in the minor `prelude-mode`, that are conflicting with the major
`org-mode`:

``` elisp
(defun my-org-mode-hook ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist))
)

(add-hook 'org-mode-hook 'my-org-mode-hook)
```
