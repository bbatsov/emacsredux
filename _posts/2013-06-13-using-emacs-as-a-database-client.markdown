---
layout: post
title: "Using Emacs as a Database Client"
date: 2013-06-13 15:15
comments: true
tags:
- Packages
- Database
---

Most people that use Emacs are programmers - that's a fact!
Most programmers have to deal with relational databases - that's also a fact!

Programmers often interact with the database they have to use via some dedicated
client - be it a console or a GUI one. One fairly little known fact about Emacs
is that it features a mode that wraps around terminal database clients, called
`SQLi` (`sql-interactive-mode`). The mode allows you to interact with a
relational database from the comfort of Emacs. Sure, you can run a terminal
client like `psql` directly from `ansi-term` (for instance) as well - but if
you're using a client with no `readline` support (like the default clients for
Oracle and DB2) you'll certainly appreciate `SQLi`.

Let's play a bit with `SQLi`. To create a PostgreSQL connection start
by running `M-x sql-postgres`. You'll be prompted for username, database,
password and host and then you'll be dropped in a buffer dedicated to the
connection you've specified. Apart from being able to run all sorts of
SQL in that buffer you'll also be able to send to it SQL from `.sql`
files you're editing in Emacs.

Let's see how you can do that in a bit more detail.  First you have to
associate an SQL file[^1] with a connection. While in some `.sql` file
execute `M-x sql-set-product` and type `postgres`. Afterwards do `M-x
sql-set-sqli-buffer` and select the name of the connection buffer you
want to use (it's probably called `*SQL*` if you have only one
connection buffer).  Now you'll be able to use commands like
`sql-send-region` (`C-c C-r`) from the `.sql` buffer and the code from
the region will be executed in the associated connection buffer.

If you're connecting to the same database most of the time you can specify the login
params in your configuration like this:

``` emacs-lisp
(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))
```

Note that there are similar variables for every supported database (e.g. `sql-mysql-login-params`, `sql-db2-login-params`, etc.)

Alternatively, you can specify a list of databases that you typically connect to and use the command `sql-connect` to select a database from the list. Here's an example:

``` emacs-lisp
(setq sql-connection-alist
      '((pgsql-prod (sql-product 'postgres)
                    (sql-port 5432)
                    (sql-server "db.prod.com")
                    (sql-user "user")
                    (sql-password "password")
                    (sql-database "my-app"))
        (pgsql-staging (sql-product 'postgres)
                       (sql-port 5432)
                       (sql-server "db.staging.com")
                       (sql-user "user")
                       (sql-password "password")
                       (sql-database "my-app"))
        (mysql-dev (sql-product 'mysql)
                   (sql-port 5432)
                   (sql-server "localhost")
                   (sql-user "user")
                   (sql-password "password")
                   (sql-database "some-app"))))
```

Once this has been evaluated, you can run `M-x sql-connect` and pick the database to
connect to. It's a good habit to rename the `SQLi` buffers manually to something
more meaningful if you have several of those - after all names like `*SQL*`, `*SQL<1>*`, etc can be pretty confusing. You can also create some simple wrapper commands that
target some particular connection to help with this:

``` emacs-lisp
(defun sql-connect-to-pqsql-prod ()
  (interactive)
  (sql-connect 'pqsql-prod "*pgsql-prod*"))
```

Obviously, you can make such wrapper functions as sophisticated as you need them to be. This one is super basic and will only connect to a particular database and set the name of the resulting `SQLi` buffer accordingly.

A word of caution - make sure you don't commit important credentials in plain text alongside your Emacs configuration. Normally in such cases you'd be using either encrypted credentials or you'll be getting the credentials from env variables.

There's a lot more to be said about using Emacs as a database client, but that's beyond the scope of this introduction article. Have a look at the documentation of `sql-mode` and `sql-interaction-mode` for further details. Also - don't forget that `sql-interaction-mode` is implemented on top of the standard `comint` package, so it supports all the common `comint` commands and configuration options.

That's all I have for you today. Keep hacking!

[^1]: Technically speaking - a buffer visiting an SQL file.
