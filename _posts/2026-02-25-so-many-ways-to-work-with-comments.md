---
layout: post
title: So Many Ways to Work with Comments
date: 2026-02-25 11:30 +0200
tags:
- Editing
---

I've been using Emacs for over 20 years and I _still_ keep discovering (and
rediscovering) comment-related commands and variables. You'd think that after two
decades I'd have comments figured out, but it turns out there's a surprising
amount of depth hiding behind a few keybindings.

What prompted this article was my recent work on
[neocaml](https://github.com/bbatsov/neocaml/), a tree-sitter based major mode
for OCaml. OCaml uses `(* ... *)` block comments -- no line comments at all --
and that unusual syntax forced me to dig deeper into how Emacs handles comments
internally. I learned more about comment variables in the past few months than in
the previous 20 years combined.

<!--more-->

## The Swiss Army Knife: M-;

I wrote about `comment-dwim` back in my [Comment Commands
Redux]({% post_url 2020-06-10-comment-commands-redux %}) article, but I don't
think I did it justice. `M-;` is genuinely one of the most context-sensitive
commands in Emacs. Here's a breakdown of what it does depending on _where_ you
invoke it:

**With an active region:** It calls `comment-region` to comment out the selected
code. But if the region already consists entirely of comments, it calls
`uncomment-region` instead. So it's effectively a toggle.[^1]

**On an empty line:** It inserts a comment (using `comment-start` and
`comment-end`) and places point between the delimiters, properly indented.

**On a line with code but no comment:** It adds an end-of-line comment, indented
to `comment-column` (default 32). This is the classic "inline comment"
workflow -- write your code, hit `M-;`, type your annotation.

**On a line that already has an end-of-line comment:** It jumps to that comment
and reindents it. Pressing `M-;` again just keeps you there.

**With a prefix argument (`C-u M-;`):** It kills the first comment on the
current line.

That's five distinct behaviors from a single keybinding. No wonder people
find it confusing at first. If you want something simpler, `comment-line` (`C-x
C-;`, added in Emacs 25.1) just toggles comments on the current line or region
-- nothing more, nothing less.

## Continuing Comments: M-j

I also wrote about `M-j` years ago in [Continue a Comment on the Next
Line]({% post_url 2013-06-05-continue-a-comment-on-the-next-line %}). The
command (`comment-indent-new-line`[^2]) breaks the current line and
continues the comment on the next line with proper indentation.

For languages with line comments (`//`, `#`, `;;`), this works great out of the
box -- it just inserts the comment delimiter on the new line. But for languages
with block comments like OCaml's `(* ... *)`, the default behavior is less
helpful: it _closes_ the current comment and opens a new one:

```
(* some text about something. *)
(* |
```

What you actually want is to continue the same comment:

```
(* some text about something.
   |
```

This is controlled by two variables that I suspect most people have never heard of:

- **`comment-multi-line`** -- when non-nil, tells commands like `M-j` to
  continue the current comment rather than closing and reopening it.

- **`comment-line-break-function`** -- the function that `M-j` actually calls to
  do its work. Major modes can set this to customize the line-breaking behavior
  inside comments.

In neocaml, I set `comment-multi-line` to `t` and provide a custom
`comment-line-break-function` that uses tree-sitter to find the column where the
comment body text starts, then indents the new line to match:

``` emacs-lisp
(setq-local comment-multi-line t)
(setq-local comment-line-break-function #'neocaml--comment-indent-new-line)
```

The implementation is straightforward -- walk up the tree-sitter AST to find the
enclosing comment node, compute the body column from the opening delimiter, and
indent accordingly. Now `M-j` inside `(** documentation *)` produces a new line
indented to align with the text after `(**`.

## Filling Comments: M-q

While I was at it I also had to teach `M-q` (`fill-paragraph`) about OCaml
comments. By default, `fill-paragraph` doesn't know where a `(* ... *)` comment
starts and ends, so it either does nothing useful or mangles things.

The fix was setting `fill-paragraph-function` to a custom function that uses
tree-sitter to find the comment boundaries, narrows to the body text (excluding
the `(*` and `*)` delimiters), and fills within that region. The fill prefix is
computed from the body start column so continuation lines align properly:

```
(* This is a long comment that gets
   wrapped at the fill column, with
   continuation lines properly
   indented *)
```

## The Comment Variable Zoo

Working on all of this made me realize just how many comment-related variables
Emacs exposes for major modes to configure. Here are the ones I ended up caring
about:

| Variable | Purpose |
|---|---|
| `comment-start` | Opening delimiter (`"(* "` for OCaml) |
| `comment-end` | Closing delimiter (`" *)"` for OCaml) |
| `comment-start-skip` | Regexp to skip the opening delimiter |
| `comment-multi-line` | Continue comments vs. close-and-reopen |
| `comment-line-break-function` | What `M-j` calls |
| `comment-column` | Column for end-of-line comments |
| `comment-style` | How `comment-region` formats comments |
| `fill-paragraph-function` | Custom fill behavior |

Most of these have sensible defaults for line-comment languages, which is why you
can go 20 years without thinking about them. But the moment you deal with block
comments, especially unusual ones like OCaml's nested `(* ... *)`, you
discover the full machinery.

## Closing Thoughts

I find it remarkable that after all these years Emacs can still surprise me. A
handful of comment commands -- `M-;`, `M-j`, `M-q` -- and a set of buffer-local
variables give you a comment editing experience that's both powerful and
deeply customizable. Most users never need to think about the underlying
variables, but they're there when you need them, and they compose nicely.

If you're a major mode author, pay attention to these variables. Getting comments
right is one of those things that users notice immediately when it breaks and
never think about when it works.

That's all I have for you today. Keep hacking!

[^1]: This is the one behavior of `comment-dwim` that's somewhat inconvenient -- to uncomment the current line you have to select it first. `comment-line` (`C-x C-;`) handles this more gracefully.
[^2]: In older Emacs versions this was called `indent-new-comment-line`. The modern name is `comment-indent-new-line`, but both work.
