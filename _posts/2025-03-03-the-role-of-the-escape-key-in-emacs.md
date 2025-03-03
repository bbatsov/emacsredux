---
layout: post
title: The role of the Escape key in Emacs
date: 2025-03-03 09:50 +0200
tags:
- Keybindings
---

The `Escape` key (`ESC`) is legendary in vim, Emacs's arch-rival.
It's so commonly used (mostly to switch back to normal mode and interrupt commands in progress)
that you'll find many articles on where to remap it (e.g. to `Caps Lock`), and there are also
many keyboards that place `ESC` where `~` normally is, to make it more accessible.[^1]

In Emacs-land, however, we never really speak about `ESC`... Why so? Well, we use `C-g` to interrupt
commands, and we obviously don't have modal editing, at least not by default. Still, I think
`ESC` has its uses in Emacs, even if they are not super obvious at first. For instance there's the
`keyboard-escape-quit` command, that's described like this:

> Exit the current "mode" (in a generalized sense of the word).
> This command can exit an interactive command such as ‘query-replace’,
> can clear out a prefix argument or a region,
> can get out of the minibuffer or other recursive edit,
> cancel the use of the current buffer (for special-purpose buffers),
> or go back to just one window (by deleting all but the selected window).

Basically, it's a fancier way of doing `C-g` (`keyboard-quit`), and it's mapped to
`ESC ESC ESC` (triple escape). Not the most convenient keybinding, but still OK if your
Escape is well positioned and you'd like to avoid holding down a modifier key.[^2]
If you take a look at the keybinding in Emacs's docs, though, you'll see it's listed
as `M-ESC ESC`, rather than `ESC ESC ESC`. And this is what makes `ESC` really interesting -
it serves as a substitute for `Meta`, but you don't have to hold down `ESC` - instead `M-something`
keybindings can be triggered by pressing `ESC` and the other key sequentially. Go ahead and try
the following:

- `ESC x` (same as `M-x`)
- `ESC g g` (`goto-line`, same as `M-g g`)
- `ESC e` (`forward-sentence`, same as `M-e`)

I don't know about you, but I think this is pretty handy, especially if you're using
`macOS`, where on many keyboards the `Option` (`Meta`) keys are pretty short, or one of them
is even missing (the right one).

For me using Emacs on macOS has always been a bit of a struggle, as the `Meta`
is way more useful than `Command` (`Super`), and historically I swapped them[^3]
because of this, but then I struggled when I had to use someone else's
keyboard. (or even my own, as I normally contained this rebinding only to
Emacs). So, the ability to use `ESC` instead of `Meta` is definitely a welcome
one, and I find myself doing this quite often.

Before we wrap up consider keybindings like `M->`, `M-!` or `M-%` that require you to
hold down both `Shift` and `Meta` when typing them. I think they way more pleasant as:

- `Esc >`
- `Esc !`
- `Esc %`

Admittedly, it took me a while to get used to this, as I didn't pay much to the
`ESC` key until I was fairly far into my Emacs journey. Topics like RSI
prevention and keybinding ergonomics rarely bother young people.

So, did you know about the role of `ESC` in Emacs? Are you making use of it? If you have any other
tips to share on the subject I'd be happy to read them.

That's all I have for you today! `ESC x` forever!

[^1]: HHKB is probably the most famous example, but there are many many others that do the same.
[^2]: I'm using a dual-function keybinding for what's normally `Caps Lock` on most keyboards - it's `Control` wwhen held down and `Escape` otherwise. For me that's a good idea regardless of the editor someone's using.
[^3]: See <https://batsov.com/articles/2012/10/14/emacs-on-osx/>.
