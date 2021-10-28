---
layout: post
title: Submitting the Current Input in ido
date: 2021-10-28 08:49 +0300
tags:
- Packages
- ido
---

Today I'll follow-up on [yesterday's Selectrum article]({% post_url 2021-10-27-submitting-the-current-input-in-selectrum %}) with
a bit more details on how to submit the current input in `ido` (as opposed to the currently selected candidate).

There are 3 ways in which you can do this, depending on the underlying `ido`
command:

- if you're using `ido-find-file` (e.g. you've pressed `C-x C-f`), then you can
switch back to the regular `find-file` using `C-f`.
- similarly if you're using `ido-switch-buffer` (or something else to do with buffers), they you can switch back to the
plain old `switch-buffer` using `C-b`.
- regardless of the current `ido` command you can always use `C-j` (same as in Selectrum).

Here's a small example:

![ido_current_input_file.png](/assets/images/ido_current_input_file.png)

If I want to create a file named "blast" I can either type `C-f` to go back to the regular `find-file` command,
or `C-j` to immediately submit the text that I've already written. I guess that in almost all situations
using `C-j` is going to be the optimal course of action.

By the way, turns out that the built-in `icomplete` mode, also uses `C-j` for submitting the current input.
