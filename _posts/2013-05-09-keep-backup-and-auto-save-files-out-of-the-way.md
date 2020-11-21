---
layout: post
title: "Keep Backup and Auto-save Files Out of the Way"
date: 2013-05-09 16:51
comments: true
tags:
- Configuration
---

Emacs has two helpful features, called `auto-backup` and
`auto-save` (or at least I call them this way).

Auto-backup is triggered when you save a file - it will keep the old
version of the file around, adding a `~` to its name. So if you saved the
file `foo`, you'd get `foo~` as well.

`auto-save-mode` auto-saves a file every few seconds or every few
characters (both settings are configurable - `auto-save-interval` is
set to 300 characters by default and `auto-save-timeout` is set to 30
seconds). The auto-save files have names like `#foo#` and are deleted
automatically when you manually save a file.

Although the modes are definitely useful, many Emacs users find the
extra files they create quite annoying(especially since they rarely
resort to using them) and disable both feature to get rid of the pesky unwanted files:

``` elisp
;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
```

Even though I've never actually had any use of those backups, I still
think it's a bad idea to disable them.[^1] I find it much more prudent
to simply get them out of sight by storing them in the OS's `tmp`
directory instead.

``` elisp
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
```

Now they won't annoy you constantly, but they will still be around if
you need them. Some OSes delete everything in their `tmp` directories
on restart, so if this worries you - consider using another directory.

[Prelude](http://github.com/bbatsov/prelude) keeps auto-backup and
auto-save files in `tmp` by default.

[^1]: Most backups are eventually useful.
