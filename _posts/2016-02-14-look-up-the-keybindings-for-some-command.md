---
layout: post
title: "Look up the keybindings for some command"
date: 2016-02-14 18:10
comments: true
tags:
- Keybindings
- Help
- Built-in Commands
---

If you know the name of some command, but you've forgotten its
keybinding(s) there are 3 common options to look the keybinding(s) up.

* `C-h f command-name`

This will display some information about the command in question in a help buffer.
Important bits of this information include where is the command defined, what are its
keybindingings if any, and its documentation.

* `C-h w command-name`

This will display the keybindings of the command in the minibuffer. If you're interested only
in the keybindings you should prefer this option over `C-h f`.

* `M-x command-name`

After you invoke some command using `M-x` you'll see a suggestion to use
its keybinding instead in the minibuffer.

That's all for now, folks!
