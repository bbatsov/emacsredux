---
layout: post
title: Submitting the Current Input in Selectrum
date: 2021-10-27 15:18 +0300
tags:
- Packages
- Selectrum
---

One thing that's always a bit annoying with minibuffer completion tools (e.g. `ido`, `ivy`, `selectrum`) is that from time to time
you just want to submit the current input, but it happens to match some candidate and it gets
selected instead. That's what happened to me the other day when I wanted to create a new tag named
`0.9.0`, but Selectrum was matching the existing tags like:

- 0.9.0-beta1
- 0.9.0-beta2
- 0.9.0-beta3

![selectrum_current_input1.png](/assets/images/selectrum_current_input1.png)

I hope you get the idea. Every minibuffer completion framework has some keybinding for this situation, but
I started using Selectrum only a few months ago and I keep forgetting its. And that's why I'm writing this blog
post - I hope it will help me remember it once and for all. So, what's the magical keybinding? There are a couple of options, actually.

To submit what you've typed, even if it's not a candidate, you can use `<up>` or
`C-p` to select the current input just like a regular candidate, and type `RET` as
usual. Alternatively, you can type `C-j` to submit your exact input without
selecting it first. I prefer `C-j`, as it's a bit more efficient, but both approaches
work just fine. Here's how this looks if you use `<up>`/`C-p`:

![selectrum_current_input2.png](/assets/images/selectrum_current_input2.png)

Notice how now the current input is highlighted as the current candidate. At this point you can just press `RET` and have it accepted.

By the way, you can do the same thing in `ivy` with `C-M-j`. I'm pretty sure I
kept pressing `C-M-j` with Selectrum, and as you can imagine it didn't do
anything there. If I recall correctly in `ido` the magic keybinding is `C-f`. If that's not the
case - please, correct me in the comments.

That's all I have for you today. A short post for a small tip. I hope I'll finally remember `C-j`!
