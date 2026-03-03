---
layout: post
title: Inline Completion with completion-preview-mode
date: 2026-02-28 09:10 +0200
tags:
- Emacs 30
- Built-ins
---

For the longest time, if you wanted inline completion suggestions in Emacs (think
ghost text that appears as you type), you'd reach for third-party packages like
`company` or `corfu`. Emacs 30 changes the game by introducing a built-in
solution: `completion-preview-mode`.

## What it does

As you type, `completion-preview-mode` shows a grayed-out preview of the most
likely completion candidate right after your cursor — inline, in the buffer
itself. No popup, no minibuffer, no fuss. If the suggestion is what you want,
hit `TAB` to accept it. If not, just keep typing and the preview updates.

## Enabling it

For a single buffer:

```emacs-lisp
(completion-preview-mode 1)
```

Or globally:

```emacs-lisp
(global-completion-preview-mode 1)
```

## Key bindings

When a preview is showing:

| Key | Action |
|-----|--------|
| `TAB` | Accept the preview |
| `M-i` | Trigger standard completion |
| Scroll up/down | Cycle through candidates |

## Configuration

A few useful options to fine-tune the behavior:

```emacs-lisp
;; Only show preview after typing at least 3 characters (default)
(setq completion-preview-minimum-symbol-length 3)

;; Add a small delay before showing the preview
(setq completion-preview-idle-delay 0.2)

;; Only show preview when there's exactly one candidate
(setq completion-preview-exact-match-only t)
```

## Should you ditch Company/Corfu?

Probably not — at least not yet. `completion-preview-mode` is intentionally
minimal. It doesn't offer a candidate list popup, annotations, or the rich
ecosystem of backends that `company` and `corfu` provide. Think of it as a
lightweight complement rather than a replacement.

That said, if you're someone who values simplicity and prefers to stick with
built-in functionality, `completion-preview-mode` is a welcome addition to the
toolkit.

That's all I have for you today. Keep hacking!
