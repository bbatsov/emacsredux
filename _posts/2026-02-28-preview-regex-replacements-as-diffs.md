---
layout: post
title: Preview Regex Replacements as Diffs
date: 2026-02-28 22:51 +0200
tags:
- Emacs 30
---

If you've ever hesitated before running `query-replace-regexp` across a large
file (or worse, across many files), you're not alone. Even experienced Emacs
users get a bit nervous about large-scale regex replacements. What if the regex
matches something unexpected? What if the replacement is subtly wrong?

Emacs 30 has a brilliant answer to this anxiety: `replace-regexp-as-diff`.

## How it works

Run `M-x replace-regexp-as-diff`, enter your search regexp and replacement
string, and instead of immediately applying changes, Emacs shows you a **diff
buffer** with all proposed replacements. You can review every single change in
familiar unified diff format before committing to anything.

If you're happy with the changes, you can apply them as a patch. If something
looks off, just close the diff buffer and nothing has changed.

## Multi-file support

It gets even better. There are two companion commands for working across files:

- `multi-file-replace-regexp-as-diff` — prompts you for a list of files and
  shows all replacements across them as a single diff.
- `dired-do-replace-regexp-as-diff` — works on marked files in Dired. Mark the
  files you want to transform, run the command, and review the combined diff.

The Dired integration is particularly nice — mark files with `m`, run the command
from the Dired buffer, and you get a comprehensive preview of all changes.

Note to self - explore how to hook this into Projectile.

## A practical example

Say you want to rename a function across your project. In Dired:

1. Mark all relevant files with `m` (or `% m` to mark by regexp)
2. Run `dired-do-replace-regexp-as-diff`
3. Enter the search pattern: `\bold_function_name\b`
4. Enter the replacement: `new_function_name`
5. Review the diff, apply if it looks good

No more sweaty palms during large refactorings.[^1]

## Closing Thoughts

I have a feeling that in the age of LLMs probably few people will get
excited about doing changes via patches, but it's a pretty cool
workflow overall. I love reviewing changes as diffs and I'll
try to incorporate some of the commands mentioned in this article
in my Emacs workflow.

That's all I have for you today. Keep hacking!

[^1]: Assuming you're still doing any large-scale refactorings "old-school", that is. And that you actually read the diffs carefully!
