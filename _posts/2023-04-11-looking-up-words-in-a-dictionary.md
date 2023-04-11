---
layout: post
title: Looking Up Words in a Dictionary
date: 2023-04-11 12:08 +0300
tags:
- Emacs 28
---

In recent years I grew quite used to the functionality presented by e-readers, some browsers and other tools to look up quickly a word in a dictionary while you're reading something. English is not my first language and from time to time I come across something I don't know, so I appreciate being able to figure it out quickly.

Turns out Emacs 28 has introduced some pretty similar functionality with the command `dictionary-lookup-definition` that will lookup the word at point. You can bind this command to something like `C-c l` (`l` for "lookup"):

``` emacs-lisp
(global-set-key (kbd "C-c l") #'dictionary-lookup-definition)
```

This command is part of the much bigger `dictionary` package, that is full of all sorts of features - e.g. a fully fledged `dictionary-mode` where you can search for words (you can start it with `M-x dictionary`). Looks of cool features there, but I need only the lookup word at point functionality.

One thing to keep in mind is that by default Emacs will try to use a locally installed dictionary server (`dictd`) and fallback to `dict.org` if such a server is not available. Installing the server is quite easy (the instructions below are for Debian and friends):

``` shellsession
$ sudo apt-get install dictd dict dict-{wn,vera,jargon,devil,gcide,foldoc}
$ sudo systemctl enable dictd
```

The above command will install the dictionary server and some common dictionaries.
But if you're lazy like me, you can just force the use of `dict.org` all the time:

``` emacs-lisp
(setq dictionary-server "dict.org")
```

You've got the following options for `dictionary-server`:

- Automatic: First try `localhost`, then dict.org after confirmation (default)
- localhost: Only use `localhost`
- dict.org: Only use dict.org
- User-defined: You can specify your own server here (e.g. "mydict.org")

Chose wisely!

That's all I have for you today. Now you have one less reason to leave the comfort of Emacs and you'll get to learn a lot of new words quickly! Keep hacking!
