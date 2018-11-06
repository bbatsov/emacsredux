---
layout: post
title: "A peek at Emacs 24.4: delete-duplicate-lines"
date: 2014-03-01 17:33
comments: true
tags:
- Emacs24.4
---

Emacs 24.4 introduces the command `delete-duplicate-lines`.

By default `M-x delete-duplicate-lines` will delete all but one copy
of any identical lines in the region. So assuming this is the region:

```
test
dup
dup
one
two
one
three
one
test
five
```

it will be reduced to this:

```
test
dup
one
two
three
five
```

When the command is invoked with a `C-u` prefix, it searches backwards
and keeps the last instance of each repeated line. So this:

```
test
dup
dup
one
two
one
three
one
test
five
```

will become:

```
dup
two
three
one
test
five
```

Identical lines need not be adjacent, unless you've invoked the
command with a `C-u C-u` prefix.  When the command is invoked with a
`C-u C-u C-u` prefix, it retains repeated blank lines.
