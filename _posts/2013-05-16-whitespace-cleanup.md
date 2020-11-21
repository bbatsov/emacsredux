---
layout: post
title: "Whitespace cleanup"
date: 2013-05-16 17:30
comments: true
tags:
- Editing
---

Good developers are very careful about the proper use of
whitespace in their files - there should be no empty lines at the
beginning of a file, no empty lines at the end of a file, no trailing
whitespace, no mixture of tabs and spaces, etc.

Emacs, naturally, wants to help with every problem possible and
provides a very nice solution in the form of the `whitespace-cleanup`
command. It's a much more powerful alternative to the older
`delete-trailing-whitespace` command, that simply deletes trailing
whitespace, and it's aware of the `whitespace-style` variable, used by `whitespace-mode`.

It usually applies to the whole buffer, but in `transient-mark-mode`
when the mark is active, it applies to the region.  It also
applies to the region when it is not in transient mark mode, the
mark is active and `C-u` was pressed just before
calling `whitespace-cleanup` interactively. There is also a similar
command called `whitespace-cleanup-region`.

The problems cleaned up are (borrowed from the official documentation):

1. Empty lines at beginning of buffer.

2. Empty lines at end of buffer.
   If `whitespace-style` includes the value `empty`, remove all
   empty lines at beginning and/or end of buffer.

3. 8 or more SPACEs at beginning of line.
   If `whitespace-style` includes the value `indentation`:
   replace 8 or more SPACEs at beginning of line by TABs, if
   `indent-tabs-mode` is non-nil; otherwise, replace TABs by
   SPACEs.
   If `whitespace-style` includes the value `indentation::tab`,
   replace 8 or more SPACEs at beginning of line by TABs.
   If `whitespace-style` includes the value `indentation::space`,
   replace TABs by SPACEs.

4. SPACEs before TAB.
   If `whitespace-style` includes the value `space-before-tab`:
   replace SPACEs by TABs, if `indent-tabs-mode` is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style` includes the value
   `space-before-tab::tab`, replace SPACEs by TABs.
   If `whitespace-style` includes the value
   `space-before-tab::space`, replace TABs by SPACEs.

5. SPACEs or TABs at end of line.
   If `whitespace-style` includes the value `trailing`, remove
   all SPACEs or TABs at end of line.

6. 8 or more SPACEs after TAB.
   If `whitespace-style` includes the value `space-after-tab`:
   replace SPACEs by TABs, if `indent-tabs-mode` is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style` includes the value
   `space-after-tab::tab`, replace SPACEs by TABs.
   If `whitespace-style` includes the value
   `space-after-tab::space`, replace TABs by SPACEs.

It might be a good idea to add `whitespace-cleanup` to your
`before-save-hook` so that every buffer would be cleaned up before it's saved:

``` elisp
(add-hook 'before-save-hook 'whitespace-cleanup)
```

For programming languages that rely on tabs being tabs you should
enable `indent-tabs-mode` (as noted above). Here's an example for
`makefile-mode`:

``` elisp
(add-hook 'makefile-mode-hook 'indent-tabs-mode)
```

[Prelude](https://github.com/bbatsov/prelude) adds
`whitespace-cleanup` to `before-save-hook` by default.
