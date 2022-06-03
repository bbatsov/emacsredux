---
layout: post
title: Extract Version Metadata from a Package
date: 2022-06-03 10:01 +0300
tags:
- Utils
- Emacs Lisp
---

Most Emacs packages need some command to display their version, as that's useful
for debugging purposes (e.g. when submitting a bug report). In the (very) old days each
package would have simply some variable with a name like `package-version` that a command like `package-display-version` would display somehow (typically in the minibuffer). It doesn't get simpler than this.

Then it became popular to have all sorts of package metadata in the comment headers of the Emacs packages. Here's an example:

``` emacs-lisp
;; Author: Bozhidar Batsov <bozhidar@nospam.dev>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience
;; Version: 2.6.0-snapshot
;; Package-Requires: ((emacs "25.1"))
```

But this opened up the question of how to best extract such metadata... `lisp-mnt.el` (bundled with Emacs) has some handy functions that can help:

- `lm-author`
- `lm-keywords`
- `lm-version`

and so on. I hope you get the idea - there's basically a function for each common metadata property. All of those operate either on the current buffer or an explicitly specified Elisp file. This means that are not very useful when you want to extract metadata from an installed package (usually installed via `package.el`), as you have to do a bit of extra work to get a handle of the files in the package.

For a few years I used in all my packages the excellent package [pkg-info](https://github.com/emacsorphanage/pkg-info), but these days both it and its underlying library [epl](https://github.com/cask/epl) are abandoned, so a while ago I decided I should replace them with something else - ideally, some standard `package.el` functionality.

`package-get-version` was the best solution that I could find. Here's the description of what the function does:

> Return the version number of the package in which this is used.
> Assumes it is used from an Elisp file placed inside the top-level directory of an installed ELPA package.
> The return value is a string (or nil in case we can’t find it).

As the function was introduced only in Emacs 27 it might not be an option for everyone. That's how I typically leverage it in most of my projects these days.

``` emacs-lisp
;;; Version information

(defconst projectile-version "2.6.0-snapshot"
  "The current version of Projectile.")

(defun projectile--pkg-version ()
  "Extract Projectile's package version from its package metadata."
  ;; Use `cond' below to avoid a compiler unused return value warning
  ;; when `package-get-version' returns nil. See #3181.
  ;; FIXME: Inline the logic from package-get-version and adapt it
  (cond ((fboundp 'package-get-version)
         (package-get-version))))

;;;###autoload
(defun projectile-version (&optional show-version)
  "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer."
  (interactive (list t))
  ((let ((version (or (projectile--pkg-version) projectile-version))))
   (if show-version
       (message "Projectile %s" version)
     version)))
```

As you can see I still use a `projectile-version` fallback var for the cases where
`package-get-version` fails for one reason or another (e.g. a package wasn't installed using `package.el`). You can also see I'm considering inlining `package-get-version` and adapting it code to make it a bit more flexible. Still, I think the basic solution outlined here is good enough for most people. For everyone else, there's the bullet-proof approach of `magit-version`:

``` emacs-lisp
(defun magit-version (&optional print-dest)
  "Return the version of Magit currently in use.
If optional argument PRINT-DEST is non-nil, output
stream (interactively, the echo area, or the current buffer with
a prefix argument), also print the used versions of Magit, Git,
and Emacs to it."
  (interactive (list (if current-prefix-arg (current-buffer) t)))
  (let ((magit-git-global-arguments nil)
        (toplib (or load-file-name buffer-file-name))
        debug)
    (unless (and toplib
                 (member (file-name-nondirectory toplib)
                         '("magit.el" "magit.el.gz")))
      (let ((load-suffixes (reverse load-suffixes))) ; prefer .el than .elc
        (setq toplib (locate-library "magit"))))
    (setq toplib (and toplib (magit--straight-chase-links toplib)))
    (push toplib debug)
    (when toplib
      (let* ((topdir (file-name-directory toplib))
             (gitdir (expand-file-name
                      ".git" (file-name-directory
                              (directory-file-name topdir))))
             (static (locate-library "magit-version.el" nil (list topdir)))
             (static (and static (magit--straight-chase-links static))))
        (or (progn
              (push 'repo debug)
              (when (and (file-exists-p gitdir)
                         ;; It is a repo, but is it the Magit repo?
                         (file-exists-p
                          (expand-file-name "../lisp/magit.el" gitdir)))
                (push t debug)
                ;; Inside the repo the version file should only exist
                ;; while running make.
                (when (and static (not noninteractive))
                  (ignore-errors (delete-file static)))
                (setq magit-version
                      (let ((default-directory topdir))
                        (magit-git-string "describe"
                                          "--tags" "--dirty" "--always")))))
            (progn
              (push 'static debug)
              (when (and static (file-exists-p static))
                (push t debug)
                (load-file static)
                magit-version))
            (when (featurep 'package)
              (push 'elpa debug)
              (ignore-errors
                (--when-let (assq 'magit package-alist)
                  (push t debug)
                  (setq magit-version
                        (and (fboundp 'package-desc-version)
                             (package-version-join
                              (package-desc-version (cadr it))))))))
            (progn
              (push 'dirname debug)
              (let ((dirname (file-name-nondirectory
                              (directory-file-name topdir))))
                (when (string-match "\\`magit-\\([0-9].*\\)" dirname)
                  (setq magit-version (match-string 1 dirname)))))
            ;; If all else fails, just report the commit hash. It's
            ;; better than nothing and we cannot do better in the case
            ;; of e.g. a shallow clone.
            (progn
              (push 'hash debug)
              ;; Same check as above to see if it's really the Magit repo.
              (when (and (file-exists-p gitdir)
                         (file-exists-p
                          (expand-file-name "../lisp/magit.el" gitdir)))
                (setq magit-version
                      (let ((default-directory topdir))
                        (magit-git-string "rev-parse" "HEAD"))))))))
    (if (stringp magit-version)
        (when print-dest
          (princ (format "Magit %s%s, Git %s, Emacs %s, %s"
                         (or magit-version "(unknown)")
                         (or (and (ignore-errors
                                    (magit--version>= magit-version "2008"))
                                  (ignore-errors
                                    (require 'lisp-mnt)
                                    (and (fboundp 'lm-header)
                                         (format
                                          " [>= %s]"
                                          (with-temp-buffer
                                            (insert-file-contents
                                             (locate-library "magit.el" t))
                                            (lm-header "Package-Version"))))))
                             "")
                         (magit--safe-git-version)
                         emacs-version
                         system-type)
                 print-dest))
      (setq debug (reverse debug))
      (setq magit-version 'error)
      (when magit-version
        (push magit-version debug))
      (unless (equal (getenv "CI") "true")
        ;; The repository is a sparse clone.
        (message "Cannot determine Magit's version %S" debug)))
    magit-version))
```

And thought that extracting that version would be easy!

One more thing before we wrap up - dealing with MELPA package versions. As MELPA overwrites
the actual package metadata version with the snapshot version it generates (basically a timestamp), you need to tweak a bit the original code from Projectile I've showed earlier. That's how my CIDER project does the version extraction:

``` emacs-lisp
(defun cider--version ()
  "Retrieve CIDER's version.
A codename is added to stable versions."
  (if (string-match-p "-snapshot" cider-version)
      (let ((pkg-version (cider--pkg-version)))
        (if pkg-version
            ;; snapshot versions include the MELPA package version
            (format "%s (package: %s)" cider-version pkg-version)
          cider-version))
    ;; stable versions include the codename of the release
    (format "%s (%s)" cider-version cider-codename)))
```

Basically, if the "real" version has the word "snapshot" in it, we're appending the
package version to the displayed version. This makes it easier to understand what's the exact snapshot build that someone's using.

After writing this article I miss `pkg-info` even more. Dealing with package metadata should be simpler!

That's all I have for you today. Keep hacking!
