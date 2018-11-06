---
layout: post
title: "Using Emacs as a database client"
date: 2013-06-13 15:15
comments: true
tags:
- Utilities
---

Most people that use Emacs are programmers - that's a fact!
Most programmers have to deal with relational databases - that's also a fact!

Programmers often interact with the database they have to use via some
dedicated client - be it a console or a GUI one. One fairly little
known fact about Emacs is that it features a mode that wraps around
terminal database clients, called `SQLi`. The mode allows you to
interact with a relational database from the comfort of Emacs. Sure,
you can run a terminal client like `psql` directly from `ansi-term`
(for instance) as well - but if you're using a client with no
`readline` support (like the default clients for Oracle and DB2)
you'll certainly appreciate `SQLi`.

Let's play a bit with `SQLi`. To create a PostgreSQL connection start
by running `M-x sql-postgres`. You'll be prompted for username, database,
password and host and then you'll be dropped in a buffer dedicated to the
connection you've specified. Apart from being able to run all sorts of
SQL in that buffer you'll also be able to send to it SQL from `.sql`
files you're editing in Emacs.

Let's see how you can do that in a bit more detail.  First you have to
associated an SQL file with a connection. While in some `.sql` file
execute `M-x sql-set-product` and type `postgres`. Afterwards do `M-x
sql-set-sqli-buffer` and select the name of the connection buffer you
want to use (it's probably called `*SQL*` if you have only one
connection buffer).  Now you'll be able to use commands like
`sql-send-region` (`C-c C-r`) from the `.sql` buffer and the code from
the region will be executed in the associated connection buffer.

Have a look at the documentation of `sql-mode` for further details.
