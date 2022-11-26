---
layout: post
title: Editing Links in org-mode
date: 2022-11-26 12:01 +0200
tags:
- org-mode
---

Links in org-mode by default are displayed as "descriptive" links, meaning they
hide their target URLs (or a destination in general). While this looks great, it
makes it a bit tricky to figure out how you can edit their URL. There are two
easy options:

1. Just press `C-c C-l` (`org-insert-link`) while your point is within a link
and you'll be prompted to edit its URL in the minibuffer. You can use the same
command to create new links (when your point is not on an existing link).

2. You can convert the "descriptive" links to "literal" links[^1] by invoking the command
`M-x org-toggle-link-display`. You can also toggle between the two display modes for links via the mode's menu (under "Hyperlinks").

And that's all I have for you today. Admitted, I keep forgetting about this all the time, as I rarely use org-mode, which is the reason I've decided to write this short article. Keep hacking!

[^1]: As a reminder - org-mode links have the format `[[URL][DESCRIPTION]]`.
