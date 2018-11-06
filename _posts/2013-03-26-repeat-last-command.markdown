---
layout: post
title: "Repeat last command"
date: 2013-03-26 18:32
comments: true
tags:
- Editing
---

Sometimes you'll want to quickly repeat an Emacs command several times and
more often than not it won't have a convenient keybinding you can use to
do this. Enter `C-x z` (`repeat`) - it simply repeats the most
recently executed command. And the best part? After you've pressed
`C-x z` once you can continue repeating the last command simply by
pressing `z`. Vi(m) users will probably note that this is quite similar
to the `.` command there.

For instance - if you want to execute the `mark-word` command
(bound to `M-@`) a few times you can do it like this:

```
M-@
M-@
M-@
M-@
M-@
```

or like this:

```
M-@
C-x z
z
z
z
```

Neat, ah?

**P.S.** In that particular case it would probably be easier to keep
hitting `M-@`, but you get the point.
