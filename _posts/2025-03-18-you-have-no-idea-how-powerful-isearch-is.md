---
layout: post
title: You have no idea how powerful isearch is!
date: 2025-03-18 18:47 +0200
tags:
- Built-ins
- isearch
---

`isearch` is probably one of the most widely known Emacs commands. Every Emacs user
knows that they can run it using `C-s` (to search forward) and `C-r` to search backwards.
Everyone also knows they can keep pressing `C-s` and `C-r` to go over the list of matches
in the current buffer. Even at this point that's a very useful command. But that doesn't
even scratch the surface of what `isearch` can do!

After you've started `isearch` you can actually do a lot more than pressing `C-s` and `C-r`:

* Type `DEL` to cancel last input item from end of search string.
* Type `RET` to exit, leaving point at location found.
* Type LFD (`C-j`) to match end of line.
* Type `M-s M-<` to go to the first match, `M-s M->` to go to the last match. (super handy)
* Type `C-w` to yank next word or character in buffer
   onto the end of the search string, and search for it. (very handy)
* Type `C-M-d` to delete character from end of search string.
* Type `C-M-y` to yank char from buffer onto end of search string and search for it.
* Type `C-M-z` to yank from point until the next instance of a
  specified character onto end of search string and search for it.
* Type `M-s C-e` to yank rest of line onto end of search string and search for it.
* Type `C-y` to yank the last string of killed text.
* Type `M-y` to replace string just yanked into search prompt
  with string killed before it.
* Type `C-q` to quote control character to search for it.
* Type `C-x 8 RET` to add a character to search by Unicode name, with completion.
* `C-g` while searching or when search has failed cancels input back to what has
  been found successfully.
* `C-g` when search is successful aborts and moves point to starting point.

You can also toggle some settings write `isearch` is active:

* Type `M-s c` to toggle search case-sensitivity.
* Type `M-s i` to toggle search in invisible text.
* Type `M-s r` to toggle regular-expression mode.
* Type `M-s w` to toggle word mode.
* Type `M-s _` to toggle symbol mode.
* Type `M-s '` to toggle character folding.

Type `M-s SPC` to toggle whitespace matching.
In incremental searches, a space or spaces normally matches any whitespace
defined by the variable `search-whitespace-regexp`; see also the variables
`isearch-lax-whitespace` and `isearch-regexp-lax-whitespace`.

Type `M-s e` to edit the search string in the minibuffer. That one is super useful!

Also supported is a search ring of the previous 16 search strings:

* Type `M-n` to search for the next item in the search ring.
* Type `M-p` to search for the previous item in the search ring.
* Type `C-M-i` to complete the search string using the search ring.

Last, but not least - you can directly search for the symbol/thing at point:

* Type `M-s .` to search for the symbol at point. (useful in the context of programming languages)
* Type `M-s M-.` to search for the thing (e.g. word or symbol) at point.

**Tip:** You don't really have to remember all those keybindings - just remember you can press `C-h b`
to show them. (after you've started `isearch`)

Most of the above text is coming straight from the docstring of `isearch`. It's funny that I've
been using Emacs for almost 20 years, I use `isearch` numerous times every day and I still often
forget about much of its functionality.

I hope you learned something useful today! Keep searching (the Emacs docs)!
