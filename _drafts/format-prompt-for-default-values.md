---
layout: post
title: "Cleaner Default Prompts with format-prompt"
date: 2026-05-14 09:30 +0300
tags:
- Emacs 28
- Elisp
---

If you've written interactive commands, you've probably done something like this:

```emacs-lisp
(read-string (format "Host (default %s): " default-host)
             nil nil default-host)
```

You build the prompt by hand, sprinkle in a "(default %s)" snippet, and pass the
default to `read-string`. Works fine, but every package author rolls their own:
"(default X)", "[X]", "<X>", "[X is the default]" — pick your flavor. It also
means users can't restyle the default-value hint globally; if you wanted square
brackets everywhere, you'd have to patch every package individually.

Emacs 28 introduced `format-prompt` to fix exactly this:

```emacs-lisp
(read-string (format-prompt "Host" default-host)
             nil nil default-host)
```

`format-prompt` produces `"Host (default X): "` out of the box. The actual
format comes from the variable `minibuffer-default-prompt-format`, so if you'd
rather see `"Host [X]: "` you can change it once and every well-behaved prompt
across Emacs picks up the new style:

```emacs-lisp
(setopt minibuffer-default-prompt-format " [%s]")
```

A few small details worth knowing:

- If the default is nil, `format-prompt` drops the suffix entirely, so you
  don't end up with `"Host (default ): "`.
- The default can also be a list — only the first element is shown in the
  prompt, mirroring how `read-from-minibuffer` treats list defaults.
- The trailing `": "` is added for you, so don't include it in the prompt
  string yourself.
- `format-prompt` also takes `&rest format-args` after the default, so you can
  splice values into the prompt itself: `(format-prompt "Open %s" default name)`.

I bumped into this one while sketching out [neat](https://github.com/nrepl/neat),
a small nREPL client for Emacs, where I had the classic hand-rolled "(default %s):"
prompt for the host. Switching to `format-prompt` is the kind of one-line cleanup
that doesn't change behavior but quietly makes Emacs a tiny bit more consistent
for everyone.

That's all I have for you today. Keep hacking!
