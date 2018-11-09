---
layout: post
title: An easy-kill
---

## A bit of Terminology

Emacs has its own terminology for what's commonly known as copying,
pasting and cutting - namely "saving", "yanking" and "killing".[^1]

On top of this, Emacs has its own internal clipboard called the `kill-ring`.
Items that you save or kill end up there and yanking pulls items
out of there.

The kill-ring is much more than a typical clipboard, but that's a
subject for an entire post itself.

<!--more-->

## The Typical Workflow

Normally you'd mark some region and then you'd use `M-w` to save the
region to the kill-ring or `C-w` to kill the region (which would also
add it to the kill-ring). You'd also use `C-y` to yank items out of
the kill-ring. Nothing special here - it's basically `C-c`, `C-x` and
`C-v` from so many apps.

Of course, that's Emacs we're talking about and we can certainly do way better
than other apps. Enter the hero of today's episode.

## easy-kill

[easy-kill](https://github.com/leoliu/easy-kill) is an awesome package
that allows you to save up on the steps you'd normally have to take
when saving and killing stuff. It's called `easy-kill`, but could have
just as easily been named `easy-save` or `fast-kill`. Setting up the
package is very easy:

``` emacs-lisp
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))
```

**Note:** The package comes bundled with [Prelude](https://github.com/bbatsov/prelude).

This basically installs it and rebinds `M-w` (`kill-ring-save`) to
`easy-kill` and `C-M-@`/`C-M-SPC` (`mark-sexp`) to `easy-mark`. If you
apply `M-w` on a region, it will behave just as it always did. The
real fun begins when you don't have a region.

`M-w` saves in this order:

* active region (if present)
* url at point
* email at point
* current line

That's already an improvement over the standard `M-w` behavior, but we're just getting
started. `easy-kill`'s real power requires you to use `M-w` as a prefix in a more complex
interaction. Assume you're working with the following Lisp code:

``` emacs-lisp
(foo b|ar baz)
```

**Note:** `|` denotes the position of the cursor.

At this point pressing `M-w` will mark and save the entire line, but
if you follow up with `w` it will just save the word (symbol)
`foo`. Pressing `w` a second time will expand the selection to include
the following word as well. If you want to save the entire enclosing
list (`(foo bar baz)`) you can do so with `M-w l`.

The full list of save to kill-ring commands is the following:

* `M-w w`: save word at point
* `M-w s`: save sexp at point
* `M-w l`: save list at point (enclosing sexp)
* `M-w d`: save defun[^2] at point
* `M-w D`: save current defun's name
* `M-w f`: save filename at point
* `M-w b`: save `buffer-file-name` (the name of the file a buffer is
  currently visiting) or `default-directory` (the current directory of
  a buffer, in case it's not visiting a file).

There are also a bunch of keybindings that modify the current selection:

* `@`: append selection to previous kill and exit. For example, `M-w d @` will append the current function to the last kill. Useful for creating compound entries in the kill-ring.
* `C-w`: kill selection and exit.
* `+`, `-` and `1-9`: expand/shrink selection by 1 or n elements.
* `0`: shrink the selection to the initial size i.e. before any expansion.
* `SPC`: cycle through things in `easy-kill-alist` (word at point, sexp at point, etc)
* `C-SPC`: turn selection into an active region, so you can apply some other operation on the region (e.g. `indent-region`)
* `C-g`: abort.

Those are a lot of keybindings, so it might take you a while to
memorize them. You can press `?` at any time while using `easy-kill`
to get a nice help screen with all of its keybindings.

Let's go over the expansion commands in a bit more detail.
Pressing `M-w w` saves the current word, and repeated presses of `w` will expand the saving to
include the following words.

```
|This is some text.
```

Here you can press `M-w w w w w` to select the text `This is some text`,
or you can alternatively do `M-w w 3`. All commands that operate some
expandable selection operate in this fashion.

`+/-` does expanding/shrinking according to the thing that was first
selected. So for words the expansion is word-wise, for lines -
line-wise, for lists or sexps, list-wise. Here's an example - if you want to select
several consecutive lines you can do it by pressing `M-w + + + +`.

**Note:** List-wise expanding/shrinking works well in lispy modes
(Emacs Lisp, Common Lisp, Scheme, Clojure, etc), and SMIE-based[^3]
modes (Prolog, SML, Modula2, Shell, Ruby, Octave, CSS, SQL etc),
org-mode, nxml-mode and js2-mode. In other types of modes you can get
some pretty weird results.

While, I normally use `easy-kill` for saving/killing buffer contents,
some of its other commands are also super useful. Back in the day I
wrote a post on [saving the current buffer's file]({% post_url
2013-03-27-copy-filename-to-the-clipboard %}) and with `easy-kill` you
can achieve the same thing using `M-w b`.

## easy-mark

`easy-mark` is similar to `easy-kill` but marks the region immediately. It
can be a handy replacement for `mark-sexp` allowing `+/-` to do list-wise
expanding/shrinking and marks the whole sexp even when in the middle
of one.

Mark my words[^4] - that's a massive improvement over the default
behavior!

## Epilogue

I've been using `easy-kill` for a few years now and it was a game
changer for me. I can never imagine going back to the way I was doing
saving and killing before. `easy-kill` also reduced a lot my usage of
[expand-region](https://github.com/magnars/expand-region.el), which I
was historically using to quickly expand a selection to what I need
and then save/kill it.

For some reason `easy-kill` never got much attention and I hope
that this post is going to change this.

That's all for today, folks! Until next time!

[^1]: <https://english.stackexchange.com/questions/40657/how-yank-came-about-in-vi-and-emacs>
[^2]: Emacs lingo for function/method definition.
[^3]: <https://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html>
[^4]: Pun intended.
