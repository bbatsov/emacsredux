---
layout: post
title: "Removing Paired Delimiters in Emacs"
date: 2026-03-14 10:30 +0200
tags:
- Editing
---

The other day someone filed [an issue](https://github.com/bbatsov/neocaml/issues/28) against
my [neocaml](https://github.com/bbatsov/neocaml) package, reporting surprising behavior with
`delete-pair`. My first reaction was -- wait, `delete-pair`? I've been using Emacs for over 20
years and I wasn't sure I had *ever* used this command. Time for some investigation!

## What is `delete-pair`?

`delete-pair` is a built-in Emacs command (defined in `lisp/emacs-lisp/lisp.el`) that deletes a
pair of matching characters -- typically parentheses, brackets, braces, or quotes. You place
point on an opening delimiter, invoke `delete-pair`, and it removes both the opening and closing
delimiter.

Given that it lives in `lisp.el`, it was clearly designed with Lisp editing in mind
originally. And it was probably quite handy back in the day -- before `paredit` came along and
made `delete-pair` largely redundant for Lisp hackers.

Here's a simple example. Given the following code (with point on the opening parenthesis):

``` ocaml
(print_endline "hello")
```

Running `M-x delete-pair` gives you:

``` ocaml
print_endline "hello"
```

Simple and useful! Yet `delete-pair` has **no default keybinding**, which probably explains why
so few people know about it. If you want to use it regularly, you'll need to bind it yourself:

``` emacs-lisp
(global-set-key (kbd "M-s-d") #'delete-pair)
```

Pick whatever keybinding works for you, of course. There's no universally agreed upon binding for
this one.

## The Gotcha

The issue that was reported boiled down to `delete-pair` not always finding the correct matching
delimiter. It uses `forward-sexp` under the hood to find the matching closer, which means its
accuracy depends entirely on the buffer's syntax table and the major mode's parsing
capabilities. For languages with complex or unusual syntax, this can sometimes lead to the wrong
delimiter being removed -- not great when you're trying to be surgical about your edits.

## Alternatives for Pair Management

If you work with paired delimiters frequently, `delete-pair` is just one tool in a rich
ecosystem. Here's a quick overview of the alternatives:

### paredit

[paredit](https://paredit.org) is the gold standard for structured editing of Lisp code. I've
been a heavy `paredit` user for as long as I can remember -- if you write any Lisp-family
language, it's indispensable. `paredit` gives you `paredit-splice-sexp` (bound to `M-s` by
default), which removes the surrounding delimiters while keeping the contents intact. There's
also `paredit-raise-sexp` (`M-r`), which replaces the enclosing sexp with the sexp at point --
another way to get rid of delimiters. And of course, `paredit` prevents you from creating
unbalanced expressions in the first place, which is a huge win.

Once you've got `paredit` in your muscle memory, you'll never think about `delete-pair` again
(as I clearly haven't).

Let's see these commands in action. In the examples below, `|` marks the position of point.

**`paredit-splice-sexp`** (`M-s`) -- removes the surrounding delimiters:

``` emacs-lisp
;; Before (point anywhere inside the inner parens):
(foo (bar| baz) quux)

;; After M-s:
(foo bar| baz quux)
```

**`paredit-raise-sexp`** (`M-r`) -- replaces the enclosing sexp with the sexp at point:

``` emacs-lisp
;; Before:
(foo (bar| baz) quux)

;; After M-r:
(foo bar| quux)
```

Notice the difference: `splice` keeps all the siblings, `raise` keeps only the sexp at point and
discards everything else inside the enclosing delimiters.

### smartparens

[smartparens](https://github.com/Fuco1/smartparens) is the most feature-rich option and works
across all languages, not just Lisps. For unwrapping pairs, it offers a whole family of commands:

- `sp-unwrap-sexp` -- removes the enclosing pair delimiters, keeping the content
- `sp-backward-unwrap-sexp` -- same, but operating backward
- `sp-splice-sexp` (`M-D`) -- removes delimiters and integrates content into the parent expression
- `sp-splice-sexp-killing-backward` / `sp-splice-sexp-killing-forward` -- splice while killing content in one direction

Here's how the key ones look in practice:

**`sp-unwrap-sexp`** -- removes the next pair's delimiters:

``` python
# Before (point on the opening bracket):
result = calculate(|[x, y, z])

# After sp-unwrap-sexp:
result = calculate(|x, y, z)
```

**`sp-splice-sexp`** (`M-D`) -- works like paredit's splice, removes the innermost enclosing pair:

``` python
# Before (point anywhere inside the parens):
result = calculate(x + |y)

# After M-D:
result = calculate x + |y
```

**`sp-splice-sexp-killing-backward`** -- splices, but also kills everything before point:

``` python
# Before:
result = [first, second, |third, fourth]

# After sp-splice-sexp-killing-backward:
result = |third, fourth
```

I used `smartparens` for a while for non-Lisp languages, but eventually found it a bit heavy for
my needs.

### electric-pair-mode

`electric-pair-mode` is the built-in option (since Emacs 24.1) that automatically inserts
matching delimiters when you type an opening one. It's lightweight, requires zero configuration,
and works surprisingly well for most use cases. I've been using it as my go-to solution for
non-Lisp languages for a while now.

The one thing `electric-pair-mode` *doesn't* offer is any way to unwrap/remove paired
delimiters. The closest it gets is deleting both delimiters when you backspace between an
adjacent empty pair (e.g., `(|)` -- pressing backspace removes both parens). But that's it --
there's no unwrap command. That's where `delete-pair` comes in handy as a complement.

### A Note on Vim's surround.vim

Having played with Vim and its various
[surround.vim](https://github.com/tpope/vim-surround)-like plugins over the years, I have to
admit -- I kind of miss that experience in Emacs, at least for removing paired
delimiters. `surround.vim` makes it dead simple: `ds(` deletes surrounding parens, `ds"` deletes
surrounding quotes. It works uniformly across all file types and feels very natural.

In Emacs, the story is more fragmented -- `paredit` handles it beautifully for Lisps,
`smartparens` does it for everything but is a heavyweight dependency, and `electric-pair-mode`
just... doesn't do it at all. `delete-pair` is the closest thing to a universal built-in
solution, but its lack of a default keybinding and its reliance on `forward-sexp` make it a bit
rough around the edges.

If you're using `electric-pair-mode` and want a simple `surround.vim`-style "delete surrounding
pair" command without pulling in a big package, here's a little hack that does the trick:

``` emacs-lisp
(defun delete-surrounding-pair (char)
  "Delete the nearest surrounding pair of CHAR.
CHAR should be an opening delimiter like (, [, {, or \".
Works by searching backward for the opener and forward for the closer."
  (interactive "cDelete surrounding pair: ")
  (let* ((pairs '((?( . ?))
                  (?[ . ?])
                  (?{ . ?})
                  (?\" . ?\")
                  (?\' . ?\')
                  (?\` . ?\`)))
         (closer (or (alist-get char pairs)
                     (error "Unknown pair character: %c" char))))
    (save-excursion
      (let ((orig (point)))
        ;; Find and delete the opener
        (when (search-backward (char-to-string char) nil t)
          (let ((open-pos (point)))
            (delete-char 1)
            ;; Find and delete the closer (adjust for removed char)
            (goto-char (1- orig))
            (when (search-forward (char-to-string closer) nil t)
              (delete-char -1))))))))

(global-set-key (kbd "M-s-d") #'delete-surrounding-pair)
```

Now you can hit `M-s-d (` to delete surrounding parens, `M-s-d "` for quotes, etc. It's
deliberately naive -- no syntax awareness, no nesting support -- so it won't play well with
delimiters inside strings or comments (it'll happily match a paren in a comment if that's what
it finds first). But for quick, straightforward edits it gets the job done.

**TIP:** If you're looking for something closer to the `surround.vim` experience in Emacs (without going
full `smartparens`), check out [surround.el](https://github.com/mkleehammer/surround). It's a
lightweight package (available on MELPA) that provides `surround.vim`-style operations --
deleting, changing, and adding surrounding pairs -- all through a single keymap. It supports both
"inner" and "outer" text selection modes and works uniformly across file types, which makes it a
nice middle ground between `delete-pair` and `smartparens`.

### When to Use What

My current setup is:

- **Lisp languages** (Emacs Lisp, Clojure, Common Lisp, etc.): `paredit`, no contest.
- **Everything else**: `electric-pair-mode` for auto-pairing (I rarely need to unwrap something outside of Lisps)

I think `surround.el` will pair well with `electric-pair-mode`, but I discovered it only recently
and I've yet to try it out in practice.

If you want a more powerful structural editing experience across all languages, `smartparens` is
hard to beat. It's just more than I personally need outside of Lisp.

## Wrapping Up

One of the greatest aspects of Emacs is that we get to learn (or relearn) something about it
every other day. Even after decades of daily use, there are always more commands lurking in the
corners, patiently waiting to be discovered.

Now, if you'll excuse me, I'm going to immediately forget about `delete-pair` again. Keep hacking!

**Update:** A reader pointed me to [this Reddit
thread](https://www.reddit.com/r/emacs/comments/1ohr4uy/tip_use_deletepair_to_change_surroundings_similar/)
with more tips on using `delete-pair` (including using it to change surrounding
delimiters, `surround.vim`-style). Worth a read if you want to get more mileage
out of it.
