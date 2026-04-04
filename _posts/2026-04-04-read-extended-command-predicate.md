---
layout: post
title: "Declutter M-x with read-extended-command-predicate"
date: 2026-04-04 11:00 +0300
tags:
- Emacs 28
- Productivity
---

This is another article inspired by my recent cleanup of [Prelude](https://github.com/bbatsov/prelude) and my
[personal Emacs config](https://github.com/bbatsov/emacs.d), following the one on
[repeat-mode](/blog/2026/04/04/repeat-mode/). I've been going through the Emacs 28-30
changelogs looking for features I had overlooked, and this small one from Emacs 28
turned out to be a real gem.

Ever noticed how `M-x` shows you *every* command, including ones that
make no sense in your current buffer? Org commands while editing Ruby,
Magit commands in a shell buffer, that sort of thing. It's not a huge
deal if you know what you're looking for, but it adds noise to the
candidate list -- especially if you're using a completion framework
like Vertico or Ivy that shows everything at a glance.

Emacs 28 added a simple way to fix this:

``` emacs-lisp
(setq read-extended-command-predicate
      #'command-completion-default-include-p)
```

<!--more-->

With this setting, `M-x` hides commands that declare themselves
inapplicable to the current major mode from the completion candidates.
So if you're in a Python buffer, you won't see `dired-do-rename` or
`clojure-align` cluttering your results.

How does the filtering actually work? `command-completion-default-include-p`
looks at the modes a command declares it belongs to (via the `interactive`
form) or checks its `completion-predicate` symbol property. If no modes are
declared and there's no completion predicate, the command is included as
usual -- so existing commands that haven't been updated are not affected.

Emacs actually ships with three predicates you can choose from (plus `nil`
for no filtering):

- `command-completion-default-include-p` -- the safe default. Excludes
  commands tagged for other modes, includes everything else.
- `command-completion-using-modes-and-keymaps-p` -- more aggressive. Shows
  commands tagged for the current mode *plus* any command that has a
  keybinding in the buffer's active keymaps. Also always includes
  `customize-*` commands. Untagged commands without keybindings are hidden.
- `command-completion-using-modes-p` -- the strictest option. Only shows
  commands explicitly tagged for the current mode. Untagged commands are
  hidden too, so this can be quite aggressive.

I'd recommend starting with `command-completion-default-include-p` since
it's the most conservative -- it won't hide anything that hasn't explicitly
opted in to the filtering.

Package authors can declare mode affiliation by adding a mode specification
to the `interactive` form:

``` emacs-lisp
(defun my-foo-command ()
  "Do something useful in foo-mode."
  (interactive nil foo-mode)
  ...)
```

The `nil` is the interactive spec (no arguments in this case), and `foo-mode`
tells Emacs this command is only relevant in `foo-mode` buffers. If a command
applies to multiple modes, just list them all:

``` emacs-lisp
(defun cider-eval-defun-at-point ()
  "Evaluate the top-level form at point."
  (interactive nil clojure-mode clojure-ts-mode)
  ...)
```

This is handy for packages like CIDER that need to work in both the
classic `clojure-mode` and the newer Tree-sitter-based `clojure-ts-mode`.

As for how well this works in practice -- many built-in commands
already declare their applicable modes, so you'll see a noticeably
cleaner `M-x` right away. Third-party package adoption is growing
but uneven. Commands that haven't been updated will simply continue
to show up everywhere, same as before -- so there's no downside to
enabling this.

## A Note for Vertico/Orderless Users

If you followed the [Vertico sample
configuration](https://github.com/minad/vertico#configuration),
you'll find this setting already there -- commented out. It was
shipped that way because it was new at the time and some users found
the disappearing commands surprising. It's been stable for years now
and works great with Vertico, Orderless, and Marginalia. Just
uncomment it and enjoy a less noisy `M-x`.

Commands that are filtered out aren't gone -- the filtering only
affects completion candidates. If you type the full command name at
the `M-x` prompt it will still execute just fine.

That's all I have for you today. Keep hacking!
