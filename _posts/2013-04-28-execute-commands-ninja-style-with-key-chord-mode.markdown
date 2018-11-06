---
layout: post
title: "Execute commands ninja-style with key-chord-mode"
date: 2013-04-28 08:45
comments: true
tags:
- Utilities
---

One of my bigger problems as an Emacs user is that there **only so
many good keybindings** available. At some point you're out of good
options, but there are still plenty of commands you'd rather be able
to execute faster than you'd be able to do with `M-x command-name`.

[key-chord-mode](http://www.emacswiki.org/emacs/key-chord.el) to the
rescue! It's a pretty handy minor mode which allows you to bind
commands to keys that should be pressed together or consecutively(with
little time between the keystrokes - by default 0.3 seconds). This
gives you some pretty good opportunities to increase your
productivity. Assuming you've installed the mode somehow, you can start
playing with it:

``` elisp
;; key chords
(require 'key-chord)

(key-chord-define-global "BB" 'iswitchb)
(key-chord-define-global "FF" 'find-file)
(key-chord-define-global "jk" 'beginning-of-buffer)

(key-chord-mode +1)
```

The examples above illustrate global key-chords. Major mode specific
key chords are also available.

One should be careful about the key-chord he chooses - it should
definitely be something you're unlikely to type normally. This also
means that if you're writing in Emacs in several languages with common
letters, key chords that make sense in say English, might not make
sense in French (for instance). Luckily for me - I write almost
exclusively in English or some programming language (and Bulgarian and
English are nothing alike).

One other note - key chords don't play very well with
`evil-mode`(Emacs's vim emulation layer) for obvious reasons. vim is
kind of key-chord-mode's principle inspiration. If you're an `evil-mode` user you
probably don't need key-chord-mode anyways.

[Prelude](https://github.com/bbatsov/prelude) enables `key-chord-mode`
out of the box and makes some creative use of it with `jj` and `JJ`
(which I'd advise you try out if you didn't know about them).
